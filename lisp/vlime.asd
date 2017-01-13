;; vim: filetype=lisp
(asdf:defsystem #:vlime
  :description "Asynchronous Vim <-> Swank interface"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:cl-async
               #:yason
               #:babel
               #:swank
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "vlime-protocol")
                             (:file "vlime-connection")
                             (:file "vlime" :depends-on ("vlime-protocol")))))
  :in-order-to ((test-op (test-op #:vlime-test))))
