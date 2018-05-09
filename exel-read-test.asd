#|
  This file is a part of exel-read project.
|#

(defsystem "exel-read-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on (#:exel-read #:prove))
  :components ((:module "tests"
                :components ((:test-file "exel-read"))))
  :description "Test system for exel-read"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
