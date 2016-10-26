;; vim: filetype=lisp
(asdf:defsystem #:vlime
  :description "Asynchronous Vim <-> Swank interface"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async
               #:blackbird
               #:lparallel
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "package")
                             (:file "vlime" :depends-on ("package")))))
  :in-order-to ((test-op (test-op #:vlime-test))))
