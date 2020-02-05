(in-package #:cl-user)
(defpackage #:vlime-loader
  (:use #:cl))
(in-package #:vlime-loader)


(defparameter *vlime-home*
  (make-pathname :directory (pathname-directory *load-truename*)
                 :device (pathname-device *load-truename*)
                 ;; Issue #27: :HOST is needed for Windows XP (?) to build the correct path.
                 :host (pathname-host *load-truename*)))

(let ((load-vlime-src (merge-pathnames (parse-namestring "load-vlime.lisp") *vlime-home*)))
    (load load-vlime-src))

(defun read-port ()
  (format t "Enter a port: ")
  (force-output)
  (multiple-value-list (eval (read))))

(defun run (port)
  (loop
    :until (restart-case (progn (vlime:main :port port
                                            #+allegro :backend #+allegro :vlime-patched
                                            )
                                t)
             (choose-different-port (p)
               :report "Choose a different port"
               :interactive read-port
               (setf port p)
               nil))))

(run 7002)
