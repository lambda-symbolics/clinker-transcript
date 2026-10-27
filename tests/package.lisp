(defpackage #:clinker-transcript/tests
  (:documentation "Tests for the clinker-transcript library.")
  (:use #:cl)
  (:import-from #:clinker-transcript
                #:json-decode
                #:json-encode
                #:json-get
                #:json-object
                #:backend-search-call-item-p
                #:chat-reasoning-item-p
                #:family-private-item-p
                #:function-call-item-p
                #:native-compaction-item-canonicalize
                #:native-compaction-item-p
                #:reasoning-item-p
                #:tool-search-item-p
                #:function-call-output-item
                #:input-image-item
                #:input-text-item
                #:item-assistant-text
                #:user-message-item
                #:make-projection
                #:projection-items
                #:projection-append
                #:projection-replace
                #:projection-metadata-table
                #:projection-error
                #:projection-error-reason
                #:projection-error-item
                #:items-for-family
                #:validate-function-call
                #:reconcile-items
                #:reconciliation-repairs
                #:reconciliation-items
                #:reconciliation-error
                #:reconciliation-error-call-id
                #:missing-output-repair-call
                #:missing-output-repair-call-id
                #:make-compaction-plan
                #:compaction-plan-cutoff
                #:compaction-plan-items
                #:compaction-plan-unresolved-calls
                #:compaction-plan-projection)
  (:export #:run-tests))
