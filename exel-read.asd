#|
  This file is a part of exel-read project.
|#

(defsystem "exel-read"
  :version "0.1.1"
  :author ""
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"  
  :depends-on (#:str #:mnas-string #:mnas-xlsx #:xlsx #:mnas-file-dialog #:lst-arr)
;;;;  #:vlisp #:math #:ltk
  :components ((:module "src"
                :components
                ((:file "exel-read"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op "exel-read-test"))))

