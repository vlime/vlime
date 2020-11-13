(in-package #:cl-user)

(defpackage #:vlime
  (:use #:cl)
  (:export #:main
           #:try-to-load))

(in-package #:vlime)


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

(defun main (&key backend
                  (interface #(127 0 0 1))
                  (port 0)
                  port-file
                  (start-swank t)
                  (swank-interface #(127 0 0 1) swank-interface-p)
                  (swank-port 0 swank-port-p)
                  (dont-close t))
  (declare (ignore backend interface port port-file start-swank swank-interface-p swank-port-p))
  (dyn-call "SWANK" "SETUP-SERVER"
            swank-port 
            #'(lambda (port)
                (format t "Server created: (~a ~a)~%" swank-interface port))
            swank:*communication-style*
            dont-close 
            nil))
