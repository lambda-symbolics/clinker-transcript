(in-package #:clinker-transcript)

;;;; -- Ordered Projections --

(defstruct (projection-state (:constructor projection--make-state (items metadata)))
  "One jointly replaceable item collection and its metadata."
  (items nil :type structlisp:deque :read-only t)
  (metadata nil :type hash-table :read-only t))

(defclass projection ()
  ((state
    :initform (projection--make-state (structlisp:make-deque)
                                     (make-hash-table :test #'equal))
    :accessor projection--state
    :documentation "The current item collection and named EQ metadata tables."))
  (:documentation "An in-memory ordered transcript and its item metadata.

The caller provides exclusion for concurrent reads and writes. Item objects are
shared; list spines and collection storage are owned by this projection."))

(define-condition projection-error (error)
  ((reason
    :initarg :reason
    :reader projection-error-reason
    :documentation "The keyword identifying the violated projection contract.")
   (item
    :initarg :item
    :reader projection-error-item
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
  (structlisp:deque-push-back (projection-state-items (projection--state projection)) item)
  item)

(defun projection-replace (projection items)
  "Replace PROJECTION with finite sequence ITEMS and prune discarded metadata.

Prepare the replacement before changing PROJECTION. Retained item identities
keep every metadata value, including explicit NIL. Return PROJECTION."
  (projection--validate-items items)
  (let ((replacement (structlisp:make-deque))
        (retained (make-hash-table :test #'eq))
        (metadata (make-hash-table :test #'equal)))
    (structlisp:deque-append replacement items)
    (map nil (lambda (item) (setf (gethash item retained) t)) items)
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
          (projection--make-state replacement metadata)))
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

(defun projection--validate-items (items)
  "Require a finite proper list or a non-string vector of item references."
  (unless (or (and (vectorp items) (not (stringp items)))
              (and (listp items)
                   (handler-case (not (null (list-length items)))
                     (type-error () nil))))
    (error 'projection-error :reason ':invalid-items :item items))
  nil)
