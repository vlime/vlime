;; vim: filetype=lisp
(asdf:defsystem #:vlime-usocket
  :description "Asynchronous Vim <-> Swank interface (usocket backend)"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:usocket
               #:yason
               #:swank
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "vlime-protocol")
                             (:file "vlime-usocket" :depends-on ("vlime-protocol")))))
  :in-order-to ((test-op (test-op #:vlime-test))))
