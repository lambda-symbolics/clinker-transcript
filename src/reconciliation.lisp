(in-package #:clinker-transcript)

;;;; -- Call and Output Reconciliation --

(defclass missing-output-repair ()
  ((call
    :initarg :call
    :reader missing-output-repair-call
    :documentation "The shared function-call item whose output is missing.")
   (call-id
    :initarg :call-id
    :reader missing-output-repair-call-id
    :documentation "The nonempty call identifier to use for the repair output."))
  (:documentation "An intent to publish an output for an interrupted function call."))

(defclass reconciliation ()
  ((entries
    :initarg :entries
    :reader reconciliation--entries
    :documentation "Ordered item references interspersed with repair intents.")
   (repairs
    :initarg :repairs
    :reader reconciliation--repairs
    :documentation "The missing-output intents in call order."))
  (:documentation "A validated, read-only call/output projection plan.

Planning has no publication effects. The host chooses when and how to persist
repairs, then materializes a replacement projection."))

(define-condition reconciliation-error (projection-error)
  ((call-id
    :initarg :call-id
    :initform nil
    :reader reconciliation-error-call-id
    :documentation "The affected call identifier, when one is available."))
  (:documentation "Invalid call/output history or an invalid supplied repair.")
  (:report (lambda (condition stream)
             (format stream "Invalid transcript reconciliation (~A)~@[ for call ~S~]."
                     (projection-error-reason condition)
                     (reconciliation-error-call-id condition)))))

(defun function-call-output-item-p (item)
  "Return true when ITEM is a Responses function-call output."
  (and (json-object-p item)
       (equal (json-get item "type") "function_call_output")))

(defun validate-function-call (item &key preceding-items)
  "Return ITEM after rejecting an absent or repeated function-call identifier.

PRECEDING-ITEMS is a finite sequence of earlier transcript items. Non-call items
are accepted unchanged. Validate a live call before publishing its durable record."
  (when (function-call-item-p item)
    (projection--validate-items preceding-items)
    (let ((call-id (reconciliation--call-id item)))
      (when (find-if (lambda (previous)
                       (and (function-call-item-p previous)
                            (equal call-id (json-get previous "call_id"))))
                     preceding-items)
        (error 'reconciliation-error :reason ':duplicate-call
               :item item :call-id call-id))))
  item)

(defun reconcile-items (items &key repaired-output-p)
  "Return a reconciliation plan for finite item sequence ITEMS.

Preserve item identity and non-tool order. Place each contiguous call group
before its outputs in call order, retaining orphan outputs at their original
positions. Detect duplicate calls and outputs before constructing any repairs.

REPAIRED-OUTPUT-P, when supplied, recognizes a host's durable repair output.
Tolerate exactly one later output only when the first output follows its call
and this predicate accepts that first output. Keep the first output because
subsequent history may have been produced from it. Never mutate ITEMS."
  (projection--validate-items items)
  (let ((source (coerce items 'list)))
    (multiple-value-bind (calls outputs)
        (reconciliation--index source repaired-output-p)
      (let ((ordered (structlisp:make-deque))
            (repairs (structlisp:make-deque))
            (remaining source))
        (loop while remaining
              for item = (pop remaining)
              do (cond
                   ((function-call-output-item-p item)
                    (unless (gethash (reconciliation--call-id item) calls)
                      (structlisp:deque-push-back ordered item)))
                   ((function-call-item-p item)
                    (let ((group (structlisp:make-deque)))
                      (structlisp:deque-push-back group item)
                      (loop while (and remaining
                                       (function-call-item-p (first remaining)))
                            do (structlisp:deque-push-back group (pop remaining)))
                      (let ((group-items (structlisp:deque->list group)))
                        (structlisp:deque-append ordered group-items)
                        (dolist (call group-items)
                          (let* ((call-id (reconciliation--call-id call))
                                 (output (gethash call-id outputs)))
                            (unless output
                              (setf output (make-instance 'missing-output-repair
                                                          :call call
                                                          :call-id call-id))
                              (structlisp:deque-push-back repairs output))
                            (structlisp:deque-push-back ordered output))))))
                   (t
                    (structlisp:deque-push-back ordered item))))
        (make-instance 'reconciliation
                       :entries (structlisp:deque->list ordered)
                       :repairs (structlisp:deque->list repairs))))))

(defun reconciliation-repairs (reconciliation)
  "Return a fresh list of RECONCILIATION's repair intents in call order."
  (copy-list (reconciliation--repairs reconciliation)))

(defun reconciliation-items (reconciliation &key repair-output)
  "Return the planned items, resolving each intent with REPAIR-OUTPUT.

REPAIR-OUTPUT receives one missing-output-repair and must return a matching
function-call output after any required publication. Validate its result before
including it. With no callback, signal an unresolved repair and offer USE-OUTPUT.

A failed callback does not change the plan. If publication partly succeeded,
rebuild the plan from the host's durable state before retrying."
  (mapcar
   (lambda (entry)
     (if (typep entry 'missing-output-repair)
         (let ((output
                 (if repair-output
                     (funcall repair-output entry)
                     (restart-case
                         (error 'reconciliation-error
                                :reason ':missing-output
                                :item (missing-output-repair-call entry)
                                :call-id (missing-output-repair-call-id entry))
                       (use-output (output)
                         :report "Supply an output after arranging its publication."
                         output)))))
           (unless (and (function-call-output-item-p output)
                        (equal (json-get output "call_id")
                               (missing-output-repair-call-id entry)))
             (error 'reconciliation-error :reason ':invalid-repair-output
                    :item output :call-id (missing-output-repair-call-id entry)))
           output)
         entry))
   (reconciliation--entries reconciliation)))

(defun reconciliation--call-id (item)
  "Return ITEM's nonempty call identifier or signal invalid history."
  (let ((call-id (json-get item "call_id")))
    (unless (non-empty-string-p call-id)
      (error 'reconciliation-error :reason ':missing-call-id :item item))
    call-id))

(defun reconciliation--index (items repaired-output-p)
  "Validate ITEMS and return call and first-output tables as two values."
  (let ((calls (make-hash-table :test #'equal))
        (outputs (make-hash-table :test #'equal))
        (outputs-after-call (make-hash-table :test #'equal))
        (late-outputs (make-hash-table :test #'equal)))
    (dolist (item items)
      (cond
        ((function-call-item-p item)
         (let ((call-id (reconciliation--call-id item)))
           (when (gethash call-id calls)
             (error 'reconciliation-error :reason ':duplicate-call
                    :item item :call-id call-id))
           (setf (gethash call-id calls) item)))
        ((function-call-output-item-p item)
         (let ((call-id (reconciliation--call-id item)))
           (multiple-value-bind (existing present-p) (gethash call-id outputs)
             (if (not present-p)
                 (setf (gethash call-id outputs) item
                       (gethash call-id outputs-after-call)
                       (not (null (gethash call-id calls))))
                 (if (and repaired-output-p
                          (gethash call-id outputs-after-call)
                          (not (gethash call-id late-outputs))
                          (funcall repaired-output-p existing))
                     (setf (gethash call-id late-outputs) t)
                     (error 'reconciliation-error :reason ':duplicate-output
                            :item item :call-id call-id))))))))
    (values calls outputs)))
