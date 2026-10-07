(defpackage #:clinker-transcript
  (:nicknames #:transcript)
  (:use #:cl)
  (:export
   ;; support
   #:json-array-p
   #:json-decode
   #:json-encode
   #:json-get
   #:json-object
   #:json-object-p
   #:non-empty-string-p
   ;; item predicates
   #:backend-search-call-item-p
   #:chat-reasoning-item-p
   #:family-private-item-p
   #:function-call-item-p
   #:native-compaction-item-canonicalize
   #:native-compaction-item-p
   #:reasoning-item-p
   #:tool-search-item-p
   ;; item constructors and readers
   #:function-call-output-item
   #:input-image-item
   #:input-text-item
   #:item-assistant-text
   #:user-message-item
   ;; ordered projection and metadata
   #:projection
   #:make-projection
   #:projection-items
   #:projection-append
   #:projection-replace
   #:projection-metadata-table
   #:projection-error
   #:projection-error-reason
   #:projection-error-item
   #:items-for-family
   ;; call/output reconciliation
   #:function-call-output-item-p
   #:validate-function-call
   #:use-output
   #:reconcile-items
   #:reconciliation
   #:reconciliation-repairs
   #:reconciliation-items
   #:reconciliation-error
   #:reconciliation-error-call-id
   #:missing-output-repair
   #:missing-output-repair-call
   #:missing-output-repair-call-id
   ;; compaction carry-forward
   #:compaction-plan
   #:make-compaction-plan
   #:compaction-plan-cutoff
   #:compaction-plan-items
   #:compaction-plan-unresolved-calls
   #:compaction-plan-projection))
