(in-package #:cl-user)

(defpackage #:vlime
  (:use #:cl)
  (:export #:main
           #:try-to-load))

(in-package #:vlime)


(defgeneric start-server (backend host port swank-host swank-port))


(define-condition quicklisp-not-found-error (error)
  ((package :initarg :package
            :initform nil
            :reader dep-install-error-package))
  (:report
    (lambda (c s)
      (format
        s
        "Quicklisp not found. Please set up Quicklisp or install the dependencies for ~a manually.~%"
        (dep-install-error-package c)))))


(defun dyn-call (package sym &rest args)
  (apply (symbol-function (find-symbol sym package)) args))

(defun install-with-quicklisp (package)
  (when (not (find-package "QUICKLISP-CLIENT"))
    (error (make-condition 'quicklisp-not-found-error :package package)))
  (dyn-call "QUICKLISP-CLIENT" "QUICKLOAD" package))

(defun try-to-load (package)
  (handler-case
    (asdf:load-system package)
    (asdf:missing-dependency ()
      (install-with-quicklisp package))))

(defun main (vlime-home)
  (let ((swank-port-file (merge-pathnames
                           (make-pathname
                             :name (format nil "swank-port.~a"
                                           (get-universal-time)))
                           vlime-home))
        (preferred-style (dyn-call "SWANK/BACKEND" "PREFERRED-COMMUNICATION-STYLE")))
    (case preferred-style
      (:spawn
        (try-to-load :vlime-usocket)
        (dyn-call "SWANK" "START-SERVER"
                  swank-port-file :style preferred-style :dont-close t)
        (with-open-file (pf swank-port-file)
          (let* ((swank-port (read pf)))
            (start-server :usocket #(0 0 0 0) 7002 #(127 0 0 1) swank-port)))
        (delete-file swank-port-file))
      (:fd-handler
        (try-to-load :vlime-sbcl)
        (dyn-call "SWANK" "START-SERVER"
                  swank-port-file :style preferred-style :dont-close t)
        (with-open-file (pf swank-port-file)
          (let* ((swank-port (read pf)))
            (start-server :sbcl #(0 0 0 0) 7002 #(127 0 0 1) swank-port)))
        (delete-file swank-port-file))
      ;((nil)) ; TODO
      (t
        (dyn-call "VOM" "ERROR"
                  "Communication style ~s not supported." preferred-style)))))
