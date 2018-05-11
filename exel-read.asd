#|
  This file is a part of exel-read project.
|#

(defsystem "exel-read"
  :version "0.1.1"
  :author ""
  :license ""
  :depends-on (#:str #:vlisp #:math #:ltk #:mnas-string )
  :components ((:module "src"
                :components
                ((:file "exel-read"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "exel-read-test"))))
