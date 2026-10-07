(in-package #:clinker-transcript/tests)

(defun compaction-test-call (id)
  "Return a minimal correlated provider call."
  (json-object "type" "function_call" "call_id" id))

(defun compaction-test-error (thunk reason)
  "Assert that THUNK fails with the specified projection error reason."
  (check (handler-case (progn (funcall thunk) nil)
           (projection-error (condition)
             (eq (projection-error-reason condition) reason)))
         "compaction rejects ~A" reason))

(defun test-compaction-cutoff ()
  "Exercise cutoff calls, parallel groups, context closure and truthful outputs."
  (let* ((old (user-message-item "old"))
         (private (json-object "type" "reasoning" "encrypted_content" "opaque"))
         (first (compaction-test-call "a"))
         (second (compaction-test-call "b"))
         (first-output (function-call-output-item "a" "done"))
         (second-output (function-call-output-item "b" "done later"))
         (steering (user-message-item "steering"))
         (source (make-projection :items (list old private first second first-output
                                              steering second-output)))
         (plan (make-compaction-plan source :cutoff 5))
         (summary (user-message-item "summary")))
    (check (= (compaction-plan-cutoff plan) 5) "cutoff is exclusive")
    (check (equal (compaction-plan-unresolved-calls plan) (list second))
           "later output does not alter cutoff truth")
    (check (equal (compaction-plan-items plan)
                  (list private first second first-output steering second-output))
           "retain parallel call group, private context, and tail")
    (check (equal (projection-items (compaction-plan-projection
                                    plan :replacement-items (vector summary)))
                  (list summary private first second first-output second-output steering))
           "outputs follow calls in call order and steering survives")
    (let ((snapshot (compaction-plan-items plan)))
      (setf (first snapshot) nil)
      (check (eq (first (compaction-plan-items plan)) private) "detached plan spine"))
    (let ((snapshot (compaction-plan-unresolved-calls plan)))
      (setf (first snapshot) nil)
      (check (eq (first (compaction-plan-unresolved-calls plan)) second)
             "detached unresolved spine")))
  (let* ((call (compaction-test-call "pending"))
         (source (make-projection :items (list call)))
         (plan (make-compaction-plan source))
         (count 0))
    (compaction-test-error (lambda () (compaction-plan-projection plan)) ':missing-output)
    (let ((replacement
            (compaction-plan-projection
             plan :repair-output
             (lambda (repair)
               (incf count)
               (function-call-output-item (missing-output-repair-call-id repair)
                                          "interrupted; execution unknown")))))
      (check (= count 1) "caller repair policy is invoked once")
      (check (equal (json-get (second (projection-items replacement)) "output")
                    "interrupted; execution unknown")
             "caller supplies truthful repair content"))
    (check (equal (projection-items source) (list call)) "repair does not publish to source")
    (let ((output (function-call-output-item "pending" "actual result")))
      (check (equal (projection-items
                     (compaction-plan-projection
                      plan :additional-items (list output)
                      :repair-output (lambda (repair) (declare (ignore repair))
                                       (error "unnecessary repair"))))
                    (list call output))
             "actual arriving output resolves retained call without repair")))
  (let* ((output (function-call-output-item "early" "ok"))
         (middle (user-message-item "middle"))
         (call (compaction-test-call "early"))
         (source (make-projection :items (list output middle call)))
         (plan (make-compaction-plan source :cutoff 2)))
    (check (equal (compaction-plan-items plan) (list output middle call))
           "post-cutoff call retains pre-cutoff output context")
    (check (equal (projection-items (compaction-plan-projection plan)) (list middle call output))
           "early correlated output is reordered safely"))
  (let ((source (make-projection :items (list (user-message-item "done")))))
    (check (null (compaction-plan-items (make-compaction-plan source)))
           "resolved prefix can be fully compacted")
    (check (= (length (compaction-plan-items (make-compaction-plan source :cutoff 0))) 1)
           "zero cutoff retains all items")
    (check (null (projection-items (compaction-plan-projection
                                    (make-compaction-plan (make-projection)))))
           "empty plan materializes")))

(defun test-compaction-failures ()
  "Reject malformed and duplicate histories before policy effects."
  (let* ((call (compaction-test-call "a"))
         (output (function-call-output-item "a" "done"))
         (source (make-projection :items (list call output)))
         (plan (make-compaction-plan source))
         (count 0))
    (dolist (cutoff '(-1 3 1.5 "one"))
      (compaction-test-error (lambda () (make-compaction-plan source :cutoff cutoff))
                            ':invalid-cutoff))
    (dolist (case (list (list call ':duplicate-call) (list output ':duplicate-output)))
      (compaction-test-error
       (lambda () (compaction-plan-projection
                   plan :additional-items (list (first case))
                   :repair-output (lambda (repair) (declare (ignore repair)) (incf count))))
       (second case)))
    (check (zerop count) "duplicates fail before caller output policy")
    (check (equal (projection-items source) (list call output))
           "failed arrivals leave original intact")
    (dolist (items (list (list call call) (list call output output)))
      (compaction-test-error (lambda () (make-compaction-plan (make-projection :items items)))
                            (if (eq (second items) call) ':duplicate-call ':duplicate-output))))
  (let* ((call (compaction-test-call "pending"))
         (source (make-projection :items (list call)))
         (plan (make-compaction-plan source)))
    (compaction-test-error
     (lambda () (compaction-plan-projection
                 plan :repair-output (lambda (repair) (declare (ignore repair))
                                        (function-call-output-item "wrong" "unknown"))))
     ':invalid-repair-output)
    (check (handler-case
               (progn (compaction-plan-projection
                       plan :repair-output (lambda (repair) (declare (ignore repair))
                                              (error "policy failed"))) nil)
             (error () t))
           "policy failure propagates")
    (check (equal (projection-items source) (list call)) "failed repair leaves source intact")
    (dolist (invalid (list "not-items" (cons call call)))
      (compaction-test-error (lambda () (compaction-plan-projection
                                        plan :replacement-items invalid)) ':invalid-items)))
  (compaction-test-error
   (lambda () (make-compaction-plan
               (make-projection :items (list (compaction-test-call "")))))
   ':missing-call-id))

(defun test-compaction-privacy-and-purity ()
  "Exercise native/portable handoff and detached metadata across family switches."
  (let* ((old (user-message-item "old"))
         (private (json-object "type" "reasoning" "encrypted_content" "secret"))
         (call (compaction-test-call "pending"))
         (source (make-projection :items (list old private call)))
         (checkpoint (json-object "type" "compaction" "encrypted_content" "native"))
         (handoff (user-message-item "portable handoff"))
         (families (make-hash-table :test #'eq))
         (handoffs (make-hash-table :test #'eq))
         (output (function-call-output-item "pending" "unknown")))
    (setf (gethash private families) ':one
          (gethash checkpoint families) ':one
          (gethash handoff handoffs) ':one
          (gethash call (projection-metadata-table source ':annotation)) nil)
    (let* ((plan (make-compaction-plan source))
           (encoded (mapcar #'json-encode (projection-items source))))
      (setf (gethash call (projection-metadata-table source ':annotation)) "changed")
      (projection-append source (user-message-item "later"))
      (dolist (case (list (list ':one (list checkpoint private call output))
                         (list ':two (list handoff call output))))
        (let* ((replacement
                 (compaction-plan-projection
                  plan :replacement-items (list checkpoint handoff) :family (first case)
                  :item-families families :handoff-families handoffs
                  :additional-items (list output)))
               (metadata (projection-metadata-table replacement ':annotation)))
          (check (equal (projection-items replacement) (second case))
                 "family privacy and portable handoff survive compaction")
          (check (and (nth-value 1 (gethash call metadata)) (null (gethash call metadata)))
                 "metadata capture preserves explicit NIL independently")
          (setf (gethash call metadata) "replacement only")
          (check (equal (gethash call (projection-metadata-table source ':annotation)) "changed")
                 "replacement metadata does not mutate original")))
      (check (equal encoded (mapcar #'json-encode (subseq (projection-items source) 0 3)))
             "planning, filtering and materializing do not mutate shared JSON items")
      (check (= (length (projection-items source)) 4) "source arrivals are not overwritten")
      (compaction-test-error
       (lambda () (compaction-plan-projection
                   plan :replacement-items (list call) :family ':two
                   :additional-items (list output)))
       ':duplicate-call)))
  (let* ((call (compaction-test-call "repaired"))
         (repair (function-call-output-item "repaired" "interrupted"))
         (late (function-call-output-item "repaired" "late actual output"))
         (source (make-projection :items (list call repair)))
         (plan (make-compaction-plan source :repaired-output-p (lambda (item) (eq item repair)))))
    (check (equal (projection-items (compaction-plan-projection plan :additional-items (list late)))
                  (list call repair))
           "caller-approved durable repair wins over one late result")
    (compaction-test-error
     (lambda () (compaction-plan-projection plan :additional-items (list late late)))
     ':duplicate-output)))


(defun test-compaction-large-call-group ()
  "Resolve a large outstanding group using actual arrivals and caller repairs."
  (let* ((calls (loop for index below 1500 collect (compaction-test-call (write-to-string index))))
         (source (make-projection :items calls))
         (plan (make-compaction-plan source))
         (arrivals (loop for call in (reverse (subseq calls 0 750))
                         collect (function-call-output-item (json-get call "call_id") "actual")))
         (count 0)
         (replacement
           (compaction-plan-projection
            plan :additional-items arrivals
            :repair-output (lambda (repair)
                             (incf count)
                             (function-call-output-item (missing-output-repair-call-id repair)
                                                        "interrupted; unknown"))))
         (items (projection-items replacement)))
    (check (= count 750) "only still unresolved calls need caller output policy")
    (check (= (length items) 3000) "one output per call in large group")
    (check (equal (subseq items 0 1500) calls) "large group keeps call order")
    (loop for call in calls
          for output in (subseq items 1500)
          do (check (equal (json-get call "call_id") (json-get output "call_id"))
                    "large group output is correlated in call order"))
    (check (equal (projection-items source) calls) "large compaction does not publish repairs")))
