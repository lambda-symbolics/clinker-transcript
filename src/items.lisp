(in-package #:clinker-transcript)

;;;; -- Item Predicates --

(defun reasoning-item-p (item)
  "Return true when ITEM is a Responses reasoning item."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "reasoning")
       t))

(defun chat-reasoning-item-p (item)
  "Return true when ITEM carries Chat Completions thinking content."
  (and (json-object-p item)
       (string= (or (json-get item "type") "") "reasoning_content")
       t))

(defun native-compaction-item-p (item)
  "Return true when ITEM carries an opaque Responses compaction checkpoint."
  (and (json-object-p item)
       (let ((type (json-get item "type")))
         (and (stringp type)
              (not (null
                    (member type
                            '("compaction"
                              "compaction_summary"
                              "context_compaction")
                            :test #'string=)))
              (non-empty-string-p (json-get item "encrypted_content"))))
       t))

(defun native-compaction-item-canonicalize (item)
  "Canonicalize ITEM's legacy opaque compaction type when it is a JSON object."
  (when (json-object-p item)
    (let ((type (json-get item "type")))
      (when (and (stringp type)
                 (string= type "compaction_summary"))
        (setf (gethash "type" item) "compaction"))))
  item)

(defun backend-search-call-item-p (item)
  "Return true when ITEM is a server-executed backend search call."
  (and (json-object-p item)
       (let ((type (json-get item "type")))
         (and (stringp type)
              (not (null (member type '("web_search_call" "custom_tool_call")
                                 :test #'string=)))))
       t))

(defun tool-search-item-p (item)
  "Return true when ITEM is a server-executed tool search call or result."
  (and (json-object-p item)
       (let ((type (json-get item "type")))
         (and (stringp type)
              (not (null (member type '("tool_search_call"
                                        "tool_search_output")
                                 :test #'string=)))))
       t))

(defun function-call-item-p (item)
  "Return true when ITEM is a Responses function call."
  (and (json-object-p item)
       (let ((type (json-get item "type")))
         (and (stringp type) (string= type "function_call")))
       t))

(defun family-private-item-p (item)
  "Return true when ITEM can only be read by its producing model family.

Reasoning and native compaction items carry encrypted content that only
the family which produced them can decrypt, and server-executed calls
carry provider-specific state that only the executing family accepts on
replay."
  (or (reasoning-item-p item)
      (chat-reasoning-item-p item)
      (native-compaction-item-p item)
      (backend-search-call-item-p item)
      (tool-search-item-p item)))


;;;; -- Item Constructors and Readers --

(defun user-message-item (content &optional leading-content-items)
  "Return a Responses API user message containing CONTENT.

LEADING-CONTENT-ITEMS are prebuilt content parts, such as images,
placed before the text."
  (json-object
   "type" "message"
   "role" "user"
   "content"
   (coerce
    (append
     leading-content-items
     (when (non-empty-string-p content)
       (list (json-object
              "type" "input_text"
              "text" content))))
    'vector)))

(defun function-call-output-item (call-id output)
  "Return a Responses API function-call output correlated by CALL-ID."
  (json-object
   "type" "function_call_output"
   "call_id" call-id
   "output" output))

(defun item-assistant-text (item)
  "Return the visible assistant text in provider response ITEM, when present."
  (when (and (json-object-p item)
             (string= (or (json-get item "type") "") "message")
             (string= (or (json-get item "role") "") "assistant"))
    (let ((content (json-get item "content")))
      (when (vectorp content)
        (let ((parts
                (loop for part across content
                      when (and (json-object-p part)
                                (member (json-get part "type")
                                        '("output_text" "text")
                                        :test #'string=)
                                (stringp (json-get part "text")))
                        collect (json-get part "text"))))
          (when parts
            (format nil "~{~A~^~%~}" parts)))))))
