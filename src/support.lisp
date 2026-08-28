(in-package #:clinker-transcript)

;;;; -- Types --

(deftype option (type)
  "Either NIL or a value of TYPE."
  `(or null ,type))

(deftype non-empty-string ()
  "A string containing at least one character."
  '(and string (not (string 0))))

(deftype timestamp ()
  "A Common Lisp universal time."
  '(integer 0))

(deftype json-object ()
  "A decoded JSON object."
  'hash-table)

(defun non-empty-string-p (value)
  "Return true when VALUE is a string containing a non-whitespace character."
  (and (stringp value)
       (not (every (lambda (character)
                     (find character
                           '(#\Space #\Tab #\Newline #\Return #\Page)))
                   value))
       t))


;;;; -- JSON --

(defun json-object (&rest properties)
  "Return a JSON object holding alternating key and value PROPERTIES."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on properties by #'cddr
          do (setf (gethash key object) value))
    object))

(defun json-object-p (value)
  "Return true when VALUE is a decoded JSON object."
  (hash-table-p value))

(defun json-array-p (value)
  "Return true when VALUE is a decoded JSON array."
  (and (vectorp value) (not (stringp value))))

(defun json-get (object key)
  "Return KEY's value in JSON OBJECT, or NIL when it is absent."
  (and (hash-table-p object)
       (values (gethash key object))))

(defun json-encode (value)
  "Encode VALUE as compact JSON text."
  (with-output-to-string (stream)
    (yason:encode value stream)))

(defun json-decode (text)
  "Decode one JSON value from TEXT with arrays as vectors."
  (let ((yason:*parse-json-arrays-as-vectors* t))
    (yason:parse text)))
