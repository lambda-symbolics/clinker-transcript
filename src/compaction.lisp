(in-package #:clinker-transcript)

;;;; -- Pure Compaction Carry-Forward --

(defclass compaction-plan ()
  ((cutoff
    :initarg :cutoff
    :reader compaction-plan-cutoff
    :documentation "Exclusive source item count captured for compaction.")
   (source
    :initarg :source
    :reader compaction-plan--source
    :documentation "Captured source items for validating late correlations.")
   (start
    :initarg :start
    :reader compaction-plan--start
    :documentation "Earliest retained item index in the captured source.")
   (items
    :initarg :items
    :reader compaction-plan--items
    :documentation "Required chronological suffix of shared item references.")
   (unresolved-calls
    :initarg :unresolved-calls
    :reader compaction-plan--unresolved-calls
    :documentation "Calls without correlated outputs at the captured cutoff.")
   (metadata
    :initarg :metadata
    :reader compaction-plan--metadata
    :documentation "Detached source metadata tables, keyed by EQUAL names.")
   (repaired-output-p
    :initarg :repaired-output-p
    :reader compaction-plan--repaired-output-p
    :documentation "The caller's predicate for recognizing previous durable repairs."))
  (:documentation "A pure, detached carry-forward plan for replacing a projection.

Item objects are shared and must not be mutated while planning or materializing.
The caller provides exclusion while capturing the source projection."))

(defun make-compaction-plan (projection &key cutoff repaired-output-p)
  "Capture PROJECTION and return its required carry-forward plan.

CUTOFF is an exclusive item count, defaulting to the captured length. Identify
calls unresolved at that cutoff, even if their outputs occur later in the source.
Retain the suffix beginning with their earliest call group and adjacent private
context, all post-cutoff items, and any earlier correlated call/output context.
Validate the entire captured history, including duplicate correlations, before
returning. Never publish repairs or change the source projection."
  (let* ((source (projection-items projection))
         (length (length source))
         (cutoff (if (null cutoff) length cutoff)))
    (unless (and (integerp cutoff) (<= 0 cutoff length))
      (error 'projection-error :reason ':invalid-cutoff :item cutoff))
    (reconcile-items source :repaired-output-p repaired-output-p)
    (let* ((prefix-plan (reconcile-items (subseq source 0 cutoff)
                                         :repaired-output-p repaired-output-p))
           (unresolved (mapcar #'missing-output-repair-call
                               (reconciliation-repairs prefix-plan)))
           (start (compaction--retained-start source cutoff unresolved)))
      (make-instance 'compaction-plan
                     :cutoff cutoff
                     :source source
                     :start start
                     :items (subseq source start)
                     :unresolved-calls unresolved
                     :metadata (compaction--copy-metadata
                                (projection-state-metadata (projection--state projection)))
                     :repaired-output-p repaired-output-p))))

(defun compaction-plan-items (plan)
  "Return a fresh chronological list of PLAN's required shared item references."
  (copy-list (compaction-plan--items plan)))

(defun compaction-plan-unresolved-calls (plan)
  "Return a fresh list of shared calls lacking outputs at PLAN's cutoff."
  (copy-list (compaction-plan--unresolved-calls plan)))

(defun compaction-plan-projection (plan &key replacement-items additional-items
                                         repair-output
                                         (repaired-output-p
                                          (compaction-plan--repaired-output-p plan))
                                         (family nil family-supplied-p)
                                         item-families handoff-families)
  "Return a separate validated replacement projection for PLAN.

Place REPLACEMENT-ITEMS before the retained suffix and ADDITIONAL-ITEMS after it.
The latter are arrivals since capture. Reconcile combined correlations before
family filtering so duplicates cannot disappear behind privacy exclusions.
When FAMILY is supplied, use ITEMS-FOR-FAMILY with the supplied identity tables
and reconcile that selected history. REPAIR-OUTPUT receives a missing-output
repair and must return its correlated output. No callback is needed when actual
outputs resolve every retained call. The library performs no I/O or execution;
caller callbacks define output policy and are responsible for their own effects.

Retained source items keep detached metadata values, including explicit NIL.
Failure leaves PLAN and its original projection unchanged. Item objects remain
shared; callers must not mutate them through callbacks."
  (projection--validate-items replacement-items)
  (projection--validate-items additional-items)
  (let* ((source (append (compaction-plan--source plan)
                         (coerce additional-items 'list)))
         (start (progn
                  (reconcile-items source :repaired-output-p repaired-output-p)
                  (compaction--retained-start source (compaction-plan--start plan) nil)))
         (combined (append (coerce replacement-items 'list) (subseq source start)))
         (validated (reconcile-items combined :repaired-output-p repaired-output-p))
         (selected (if family-supplied-p
                       (reconcile-items
                        (items-for-family combined family
                                          :item-families item-families
                                          :handoff-families handoff-families)
                        :repaired-output-p repaired-output-p)
                       validated))
         (items (reconciliation-items selected :repair-output repair-output))
         (replacement (make-projection :items items)))
    ;; Validate supplied repairs together, not merely one output at a time.
    (reconcile-items items :repaired-output-p repaired-output-p)
    (setf (projection--state replacement)
          (projection--make-state
           (projection-state-items (projection--state replacement))
           (compaction--copy-metadata (compaction-plan--metadata plan) items t)))
    replacement))

(defun compaction--copy-metadata (metadata &optional items prune-p)
  "Copy METADATA tables, retaining only ITEMS when PRUNE-P is true."
  (let ((copy (make-hash-table :test #'equal))
        (retained (make-hash-table :test #'eq)))
    (dolist (item items)
      (setf (gethash item retained) t))
    (maphash
     (lambda (key table)
       (let ((entries (make-hash-table :test #'eq)))
         (maphash (lambda (item value)
                    (when (or (not prune-p) (gethash item retained))
                      (setf (gethash item entries) value)))
                  table)
         (setf (gethash key copy) entries)))
     metadata)
    copy))

(defun compaction--retained-start (source cutoff unresolved)
  "Return the earliest suffix boundary closed over correlated context."
  (let ((positions (make-hash-table :test #'equal))
        (pending (make-hash-table :test #'eq))
        (items (coerce source 'vector))
        (start cutoff))
    (dolist (call unresolved)
      (setf (gethash call pending) t))
    (loop for item across items
          for index from 0
          do (when (gethash item pending)
               (setf start (min start index)))
             (when (or (function-call-item-p item)
                       (function-call-output-item-p item))
               (let ((id (reconciliation--call-id item)))
                 (unless (nth-value 1 (gethash id positions))
                   (setf (gethash id positions) index)))))
    (loop for index downfrom (1- (length items)) to 0
          while (>= index start)
          for item = (aref items index)
          do (when (or (function-call-item-p item)
                       (function-call-output-item-p item))
               (setf start (min start (gethash (reconciliation--call-id item) positions))))
             (loop while (and (plusp start)
                              (or (function-call-item-p (aref items (1- start)))
                                  (family-private-item-p (aref items (1- start)))))
                   do (decf start)))
    start))
