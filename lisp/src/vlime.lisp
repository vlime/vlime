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

(defun main (&key backend (port 0) port-file)
  (when (not backend)
    (let ((preferred-style (dyn-call "SWANK/BACKEND" "PREFERRED-COMMUNICATION-STYLE")))
      (case preferred-style
        (:spawn
          (setf backend :vlime-usocket))
        (:fd-handler
          (setf backend :vlime-sbcl))
        ((nil)
         (setf backend :vlime-patched))
        (t
          (format *error-output*
                  "Vlime: Communication style ~s not supported.~%" preferred-style)
          (return-from main)))))

  (let ((swank-port nil)
        (swank-comm-style
          (dyn-call "SWANK/BACKEND" "PREFERRED-COMMUNICATION-STYLE")))
    (labels ((announce-swank-port (port)
               (setf swank-port port))
             (announce-vlime-port (port)
               (when port-file
                 (with-open-file (pf port-file
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
                   (with-standard-io-syntax
                     (write port :stream pf)))))
             (start-vlime-server (backend)
               (multiple-value-bind (server local-name)
                                    (start-server backend #(0 0 0 0) port #(127 0 0 1) swank-port)
                 (declare (ignore server))
                 (announce-vlime-port (nth 1 local-name)))))
      (ecase backend
        (:vlime-usocket
          (try-to-load :vlime-usocket)
          (dyn-call "SWANK" "SETUP-SERVER"
                    0 #'announce-swank-port swank-comm-style t nil)
          (start-vlime-server :usocket))
        (:vlime-sbcl
          (try-to-load :vlime-sbcl)
          (dyn-call "SWANK" "SETUP-SERVER"
                    0 #'announce-swank-port swank-comm-style t nil)
          (start-vlime-server :sbcl))
        (:vlime-patched
          (try-to-load :vlime-patched)
          (dyn-call "VLIME-PATCHED" "PATCH-SWANK")
          (dyn-call "SWANK" "SETUP-SERVER"
                    port
                    #'(lambda (port)
                        (format t "Server created: (#(127 0 0 1) ~a)~%" port)
                        (announce-vlime-port port))
                    swank-comm-style t nil))))))
