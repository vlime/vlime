;; vim: filetype=lisp
(asdf:defsystem #:vlime-sbcl
  :description "Asynchronous Vim <-> Swank interface (SBCL backend)"
  :author "Kay Z. <l04m33@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:vlime
               #:sb-bsd-sockets
               #:sb-introspect
               #:vom)
  :components ((:module "src"
                :pathname "src"
                :components ((:file "vlime-protocol")
                             (:file "vlime-connection")
                             (:file "aio-sbcl")
                             (:file "vlime-sbcl"
                              :depends-on ("vlime-protocol" "vlime-connection" "aio-sbcl")))))
  :in-order-to ((test-op (test-op #:vlime-test))))
