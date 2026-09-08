(asdf:defsystem #:clinker-transcript
  :description "Portable provider transcript items for language-model clients."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:yason #:structlisp)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "support")
                             (:file "items")
                             (:file "projection")
                             (:file "reconciliation"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:clinker-transcript/tests))))

(asdf:defsystem #:clinker-transcript/tests
  :description "Tests for clinker-transcript."
  :depends-on (#:clinker-transcript)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests")
                             (:file "projection-tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:clinker-transcript/tests '#:run-tests)))
