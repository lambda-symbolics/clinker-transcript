(in-package #:clinker-transcript)

;;;; -- Ordered Projections --

(defstruct (projection-state (:constructor projection--make-state (items metadata calls)))
  "One jointly replaceable item collection, its metadata, and its call index."
  (items nil :type structlisp:deque :read-only t)
  (metadata nil :type hash-table :read-only t)
  (calls nil :type hash-table :read-only t))

(defclass projection ()
  ((state
    :initform      (projection--make-state (structlisp:make-deque)
                                          (make-hash-table :test #'equal)
                                          (make-hash-table :test #'equal))
    :accessor      projection--state
    :documentation "The current item collection and named EQ metadata tables."))
  (:documentation "An in-memory ordered transcript and its item metadata.

The caller provides exclusion for concurrent reads and writes. Item objects are
shared; list spines and collection storage are owned by this projection."))

(define-condition projection-error (error)
  ((reason
    :initarg       :reason
    :reader        projection-error-reason
    :documentation "The keyword identifying the violated projection contract.")
   (item
    :initarg       :item
    :reader        projection-error-item
    :documentation "The item or collection which violated the contract."))
  (:documentation "An invalid projection operation.")
  (:report (lambda (condition stream)
             (format stream "Invalid transcript projection: ~A."
                     (projection-error-reason condition)))))

(defun make-projection (&key items)
  "Return a projection initialized with the finite item sequence ITEMS."
  (let ((projection (make-instance 'projection)))
    (projection-replace projection items)
    projection))

(defun projection-items (projection)
  "Return a fresh chronological list of PROJECTION's shared item references."
  (structlisp:deque->list (projection-state-items (projection--state projection))))

(defun projection-metadata-table (projection key)
  "Return the EQ item-metadata table named by EQUAL key KEY, creating it if needed.

Set entries for projected items. Replacement discards metadata for removed items
and replaces these tables, so obtain a fresh table after projection replacement."
  (let ((metadata (projection-state-metadata (projection--state projection))))
    (or (gethash key metadata)
        (setf (gethash key metadata) (make-hash-table :test #'eq)))))

(defun projection-append (projection item)
  "Append ITEM in constant amortized time and return ITEM."
  (let ((state (projection--state projection)))
    (structlisp:deque-push-back (projection-state-items state) item)
    (projection--index-call (projection-state-calls state) item))
  item)

(defun projection-replace (projection items)
  "Replace PROJECTION with finite sequence ITEMS and prune discarded metadata.

Prepare the replacement before changing PROJECTION. Retained item identities
keep every metadata value, including explicit NIL. Return PROJECTION."
  (projection--validate-items items)
  (let ((replacement (structlisp:make-deque))
        (retained    (make-hash-table :test #'eq))
        (metadata    (make-hash-table :test #'equal))
        (calls       (make-hash-table :test #'equal)))
    (structlisp:deque-append replacement items)
    (map nil (lambda (item)
               (setf (gethash item retained) t)
               (projection--index-call calls item))
         items)
    (maphash
     (lambda (key table)
       (let ((pruned (make-hash-table :test #'eq)))
         (maphash (lambda (item value)
                    (when (gethash item retained)
                      (setf (gethash item pruned) value)))
                  table)
         (setf (gethash key metadata) pruned)))
     (projection-state-metadata (projection--state projection)))
    (setf (projection--state projection)
          (projection--make-state replacement metadata calls)))
  projection)

(defun items-for-family (items family &key item-families handoff-families)
  "Return a fresh projection of ITEMS usable by FAMILY.

ITEM-FAMILIES maps item identities to producing families. Private items without
a matching family are omitted. HANDOFF-FAMILIES identifies portable handoffs
which should be omitted only for the family that has the native checkpoint."
  (remove-if
   (lambda (item)
     (or (and (family-private-item-p item)
              (not (and item-families
                        (multiple-value-bind (producer present-p)
                            (gethash item item-families)
                          (and present-p (eq producer family))))))
         (and handoff-families
              (multiple-value-bind (excluded present-p)
                  (gethash item handoff-families)
                (and present-p (eq excluded family))))))
   items))

(defun projection--index-call (calls item)
  "Record ITEM in CALLS under its call identifier when it is a function call."
  (when (function-call-item-p item)
    (let ((call-id (json-get item "call_id")))
      (when call-id
        (setf (gethash call-id calls) item))))
  nil)

(defun projection--validate-items (items)
  "Require a finite proper list or a non-string vector of item references."
  (unless (or (and (vectorp items) (not (stringp items)))
              (and (listp items)
                   (handler-case (not (null (list-length items)))
                     (type-error () nil))))
    (error 'projection-error :reason ':invalid-items :item items))
  nil)


;;;; -- Detached Transformations --

(defstruct projection-lineage
  "Call-local original identities and their current replacements."
  (origins (make-hash-table :test #'eq))
  (current (make-hash-table :test #'eq)))

(defun projection-lineage-replace (lineage item action &key replacement)
  "Record an explicit :REPLACE or :DROP of ITEM in LINEAGE in constant time."
  (unless (member action '(:replace :drop))
    (error 'projection-error :reason ':invalid-transform-action :item action))
  (let ((origin (gethash item (projection-lineage-origins lineage) item)))
    (setf (gethash item (projection-lineage-origins lineage)) origin
          (gethash origin (projection-lineage-current lineage))
          (cons action replacement))
    (when (eq action :replace)
      (setf (gethash replacement (projection-lineage-origins lineage)) origin)))
  lineage)

(defun transform-json (value &key node-function)
  "Return a detached JSON VALUE and whether it was retained.

NODE-FUNCTION runs before descent and returns (values ACTION REPLACEMENT),
where ACTION is :RETAIN, :REPLACE or :DROP. NIL is an ordinary replacement
value, not an action. Replacement subtrees are copied without further callbacks.
Copy hash keys, strings, vectors and proper lists, preserving internal sharing.
Numbers, symbols and characters are immutable scalars. Reject unsupported mutable
objects and cyclic/improper JSON containers with PROJECTION-ERROR. Callbacks
must not mutate their inputs. Dropped children are omitted."
  (funcall (projection--json-transformer) value node-function))

(defun projection--json-transformer ()
  "Return an operation-local memoized JSON copier accepting a node and callback."
  (let ((memo       (make-hash-table :test #'eq))
        (active     (make-hash-table :test #'eq))
        (raw-memo   (make-hash-table :test #'eq))
        (raw-active (make-hash-table :test #'eq)))
    (labels ((copy-node (node callback)
               (let ((memo   (if callback memo raw-memo))
                     (active (if callback active raw-active)))
                (multiple-value-bind (action replacement)
                    (if callback (funcall callback node) (values :retain nil))
                  (case action
                    (:drop
                     (values nil nil))
                    (:replace
                     (copy-node replacement nil))
                    (:retain
                     (multiple-value-bind (copy present-p) (gethash node memo)
                       (when present-p (return-from copy-node (values copy t))))
                     (when (gethash node active)
                       (error 'projection-error :reason ':cyclic-json :item node))
                     (setf (gethash node active) t)
                     (let ((copy (copy-value node callback)))
                       (remhash node active)
                       (setf (gethash node memo) copy)
                       (values copy t)))
                    (otherwise
                      (error 'projection-error :reason ':invalid-transform-action :item action))))))

             (copy-value (node callback)
               (typecase node
                 (hash-table
                  (let ((copy (make-hash-table :test (hash-table-test node))))
                    (maphash (lambda (key child)
                               (multiple-value-bind (value kept-p) (copy-node child callback)
                                 (when kept-p
                                   (setf (gethash (copy-node key nil) copy) value))))
                             node)
                    copy))
                 (string
                  (copy-seq node))
                 (cons
                  (unless (listp (rest node))
                    (error 'projection-error :reason ':invalid-json-list :item node))
                  (multiple-value-bind (head kept-p) (copy-node (first node) callback)
                    (let ((tail (copy-node (rest node) callback)))
                      (unless (listp tail)
                        (error 'projection-error :reason ':invalid-json-list :item node))
                      (if kept-p (cons head tail) tail))))
                 (vector
                  (let ((children nil))
                    (map nil (lambda (child)
                               (multiple-value-bind (value kept-p) (copy-node child callback)
                                 (when kept-p (push value children))))
                         node)
                    (coerce (nreverse children) 'vector)))
                 ((or symbol number character)
                  node)
                 (t
                  (error 'projection-error :reason ':unsupported-json-value :item node)))))
      #'copy-node)))

(defun projection--transform-correlations (items repaired-output-p)
  "Validate ITEMS without materializing or executing reconciliation repairs."
  (multiple-value-bind (calls outputs)
      (reconciliation--index (coerce items 'list) repaired-output-p)
    (values calls outputs)))

(defun projection--validate-transform-correlations
    (&key source-calls source-outputs result-calls result-outputs remap)
  "Reject new incomplete correlations and broken source-pair origins."
  (maphash
   (lambda (id call)
     (unless (or (gethash id result-outputs)
                 (and (gethash id source-calls)
                      (not (gethash id source-outputs))
                      (eq call (gethash (gethash id source-calls) remap))))
       (error 'projection-error :reason ':transform-missing-output :item id)))
   result-calls)
  (maphash
   (lambda (id output)
     (unless (or (gethash id result-calls)
                 (and (gethash id source-outputs)
                      (not (gethash id source-calls))
                      (eq output (gethash (gethash id source-outputs) remap))))
       (error 'projection-error :reason ':transform-orphan-output :item id)))
   result-outputs)
  (maphash
   (lambda (id call)
     (let ((output (gethash id source-outputs)))
       (when output
         (let* ((new-call   (gethash call remap))
                (new-output (gethash output remap))
                (call-p     (function-call-item-p new-call))
                (output-p   (function-call-output-item-p new-output)))
            (when (and (or call-p output-p
                           (function-call-output-item-p new-call)
                           (function-call-item-p new-output))
                      (not (and call-p output-p
                                (equal (json-get new-call "call_id")
                                       (json-get new-output "call_id")))))
             (error 'projection-error :reason ':transform-broken-pair :item id))))))
   source-calls)
  nil)

(defun projection-transform (source &key item-function node-function
                                        (metadata-value-function nil metadata-copier-supplied-p)
                                        lineage repaired-output-p)
  "Return a detached projection, an EQ source-to-result map, and ordered changes.

NODE-FUNCTION uses TRANSFORM-JSON's explicit action protocol. ITEM-FUNCTION
receives a detached root and its zero-based position, and returns the same action
protocol; it may edit that detached root. Root replacements inherit every named
metadata entry, including present NIL. Metadata values are detached JSON by
default; METADATA-VALUE-FUNCTION can copy application-specific opaque values.
LINEAGE optionally carries replacements across successive views by canonical
original identity. Update it only after callbacks and validation succeed.
Changes are plists with :ORIGIN, :POSITION and :ACTION. Synthesized groups are
not inferred: the host must explicitly choose their metadata policy.

Validate source and result correlations without executing repairs. Existing
missing or orphan outputs are tolerated; transformations may remove whole
exchanges but must not introduce a missing output or orphan an existing output.
Repeated EQ source roots signal PROJECTION-ERROR (:DUPLICATE-TRANSFORM-ROOT),
since identity-keyed remaps and metadata cannot represent separate occurrences.
Callbacks must not mutate source objects, opaque metadata or LINEAGE."
  (when (and metadata-copier-supplied-p (not (functionp metadata-value-function)))
    (error 'projection-error :reason ':invalid-metadata-copier :item metadata-value-function))
  (let* ((items           (projection-items source))
         (remap           (make-hash-table :test #'eq))
         (item-copier     (projection--json-transformer))
         (metadata-copier (projection--json-transformer))
         (origins         (and lineage (projection-lineage-origins lineage)))
         (current         (and lineage (projection-lineage-current lineage)))
         (pending         nil)
         (result          nil)
         (changes         nil))
    (multiple-value-bind (source-calls source-outputs)
        (projection--transform-correlations items repaired-output-p)
      ;; The identity-keyed remap and metadata tables cannot represent occurrences.
      ;; Reject repeated roots before callbacks rather than silently losing entries.
      (let ((seen (make-hash-table :test #'eq)))
        (dolist (item items)
          (when (gethash item seen)
            (error 'projection-error :reason ':duplicate-transform-root :item item))
          (setf (gethash item seen) t)))
      (loop for item in items for position from 0
            for origin = (if origins (gethash item origins item) item)
            for prior = (and current (gethash origin current))
            do (multiple-value-bind (candidate retained-p)
                   (if (eq (first prior) :drop)
                       (values nil nil)
                     (funcall item-copier (if prior (rest prior) item) node-function))
                 (multiple-value-bind (action replacement)
                     (if (not retained-p)
                         (values :drop nil)
                         (if item-function
                             (funcall item-function candidate position)
                             (values :retain nil)))
                   (unless (member action '(:retain :replace :drop))
                     (error 'projection-error :reason ':invalid-transform-action :item action))
                   (multiple-value-bind (copy kept-p)
                       (case action
                         (:drop
                          (values nil nil))
                         (:replace
                          (transform-json replacement))
                         (:retain
                          (values candidate t)))
                    (when kept-p
                      (push copy result)
                      (setf (gethash item remap) copy))
                    (push (list origin item copy kept-p) pending)
                    (push (list :origin origin :position position
                               :action (if kept-p action :drop)) changes)))))
      (setf result (nreverse result))
      (multiple-value-bind (result-calls result-outputs)
          (projection--transform-correlations result repaired-output-p)
        (projection--validate-transform-correlations
         :source-calls source-calls :source-outputs source-outputs
         :result-calls result-calls :result-outputs result-outputs :remap remap)))
    (let ((target (make-projection :items result)))
      (projection-copy-metadata source target :remap         remap
                                             :value-function
                                             (if metadata-copier-supplied-p
                                                 metadata-value-function
                                                 (lambda (value)
                                                   (funcall metadata-copier value nil))))
      (when lineage
        (dolist (entry pending)
          (destructuring-bind (origin item copy kept-p) entry
            (setf (gethash item origins) origin
                  (gethash origin current) (cons (if kept-p :replace :drop) copy))
            (when kept-p (setf (gethash copy origins) origin)))))
      (values target remap (nreverse changes)))))

(defun projection-copy-metadata (source target &key remap value-function)
  "Copy all named SOURCE metadata to TARGET, preserving present NIL.

REMAP is an EQ source-to-target root map. Without it, copy metadata for retained
EQ identities in TARGET. Values are shared unless VALUE-FUNCTION copies them.
Preserve TARGET-only entries. Prepare all tables before changing TARGET;
return TARGET."
  (let ((mapping  (or remap (make-hash-table :test #'eq)))
        (metadata (make-hash-table :test #'equal)))
    (unless remap
      (dolist (item (projection-items target)) (setf (gethash item mapping) item)))
    (maphash (lambda (key table)
               (let ((copy (make-hash-table :test #'eq)))
                 (maphash (lambda (item value) (setf (gethash item copy) value)) table)
                 (setf (gethash key metadata) copy)))
             (projection-state-metadata (projection--state target)))
    (maphash
     (lambda (key table)
       (let ((copy (or (gethash key metadata) (make-hash-table :test #'eq))))
         (maphash (lambda (item value)
                    (multiple-value-bind (replacement present-p) (gethash item mapping)
                      (when present-p
                        (setf (gethash replacement copy)
                              (if value-function (funcall value-function value) value)))))
                  table)
         (setf (gethash (if (stringp key) (copy-seq key) key) metadata) copy)))
     (projection-state-metadata (projection--state source)))
    (let ((state (projection--state target)))
      (setf (projection--state target)
            (projection--make-state (projection-state-items state) metadata
                                    (projection-state-calls state)))))
  target)
