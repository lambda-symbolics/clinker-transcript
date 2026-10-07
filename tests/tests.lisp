(defpackage #:clinker-transcript/tests
  (:use #:cl #:clinker-transcript)
  (:export #:run-tests))

(in-package #:clinker-transcript/tests)

(defvar *assertions* 0)

(defun check (value control &rest arguments)
  "Count one assertion and fail with CONTROL unless VALUE is true."
  (incf *assertions*)
  (unless value
    (error (apply #'format nil control arguments))))

(defun test-predicates ()
  "Test item type predicates over decoded provider items."
  (check (reasoning-item-p (json-object "type" "reasoning"))
         "reasoning items are recognized")
  (check (chat-reasoning-item-p (json-object "type" "reasoning_content"))
         "chat thinking items are recognized")
  (check (native-compaction-item-p
          (json-object "type" "compaction" "encrypted_content" "opaque"))
         "compaction checkpoints are recognized")
  (check (not (native-compaction-item-p (json-object "type" "compaction")))
         "compaction items require encrypted content")
  (check (backend-search-call-item-p (json-object "type" "web_search_call"))
         "backend search calls are recognized")
  (check (tool-search-item-p (json-object "type" "tool_search_output"))
         "tool search results are recognized")
  (check (function-call-item-p (json-object "type" "function_call"))
         "function calls are recognized")
  (check (not (family-private-item-p (json-object "type" "message")))
         "plain messages are portable across families")
  (dolist (type '("reasoning" "reasoning_content" "web_search_call"
                  "tool_search_call" "tool_search_output"))
    (check (family-private-item-p (json-object "type" type
                                               "encrypted_content" "x"))
           "~A items stay family private" type)))

(defun test-canonicalization ()
  "Test legacy compaction types canonicalize in place."
  (let ((item (json-object "type" "compaction_summary"
                           "encrypted_content" "opaque")))
    (check (eq (native-compaction-item-canonicalize item) item)
           "canonicalization returns the item")
    (check (string= (json-get item "type") "compaction")
           "legacy compaction summaries canonicalize to compaction"))
  (check (string= (json-get (native-compaction-item-canonicalize
                             (json-object "type" "context_compaction"))
                            "type")
                  "context_compaction")
         "modern compaction types stay untouched"))

(defun test-constructors ()
  "Test message and tool output construction plus assistant text reads."
  (let ((message (user-message-item
                  "hello"
                  (list (json-object "type" "input_image" "image_url" "u")))))
    (check (string= (json-get message "role") "user")
           "user messages carry the user role")
    (let ((content (json-get message "content")))
      (check (and (= (length content) 2)
                  (string= (json-get (aref content 0) "type") "input_image")
                  (string= (json-get (aref content 1) "text") "hello"))
             "leading content precedes the text part")))
  (check (zerop (length (json-get (user-message-item "") "content")))
         "blank content builds an empty message body")
  (let ((image (input-image-item "data:image/png;base64,AA=="))
        (low (input-image-item "data:image/png;base64,AA==" :detail "low"))
        (text (input-text-item "caption")))
    (check (and (string= (json-get image "type") "input_image")
                (string= (json-get image "image_url") "data:image/png;base64,AA==")
                (string= (json-get image "detail") "high")
                (string= (json-get low "detail") "low")
                (string= (json-get text "type") "input_text")
                (string= (json-get text "text") "caption"))
           "image and text content parts carry their URL, detail, and text"))
  (let ((output (function-call-output-item "call-1" "ok")))
    (check (and (string= (json-get output "call_id") "call-1")
                (string= (json-get output "output") "ok"))
           "function outputs correlate by call id"))
  (let ((item (json-decode
               (json-encode
                (json-object
                 "type" "message"
                 "role" "assistant"
                 "content" (vector
                            (json-object "type" "output_text" "text" "first")
                            (json-object "type" "text" "text" "second")
                            (json-object "type" "refusal" "text" "never")))))))
    (check (string= (item-assistant-text item)
                    (format nil "first~%second"))
           "assistant text joins only visible text parts"))
  (check (null (item-assistant-text (json-object "type" "reasoning")))
         "non-message items carry no assistant text"))

(defun run-tests ()
  "Run the clinker-transcript tests and return true on success."
  (setf *assertions* 0)
  (test-predicates)
  (test-canonicalization)
  (test-constructors)
  (test-projection-storage)
  (test-projection-families)
  (test-reconciliation-order)
  (test-reconciliation-validation)
  (test-reconciliation-recovery)
  (test-compaction-cutoff)
  (test-compaction-failures)
  (test-compaction-privacy-and-purity)
  (test-compaction-large-call-group)
  (format t "~&~D clinker-transcript assertions passed.~%" *assertions*)
  t)
