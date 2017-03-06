(in-package #:cl-user)
(defpackage #:vlime-loader
  (:use #:cl))
(in-package #:vlime-loader)


(defparameter *vlime-home* (make-pathname :directory (pathname-directory *load-truename*)))

(let ((load-vlime-src (merge-pathnames (parse-namestring "load-vlime.lisp") *vlime-home*)))
    (load load-vlime-src))

(vlime:main :port 7002)
