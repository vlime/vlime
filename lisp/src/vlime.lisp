(in-package #:cl-user)

(defpackage #:vlime
  (:use #:cl)
  (:export #:main))

(in-package #:vlime)


(defgeneric start-server (backend host port swank-host swank-port))


(defun main (vlime-home)
  (let ((swank-port-file (merge-pathnames
                           (make-pathname
                             :name (format nil "swank-port.~a"
                                           (get-universal-time)))
                           vlime-home))
        (preferred-style (swank/backend:preferred-communication-style)))
    (case preferred-style
      (:spawn
        (asdf:load-system :vlime-usocket)
        (swank:start-server swank-port-file :style preferred-style :dont-close t)
        (with-open-file (pf swank-port-file)
          (let* ((swank-port (read pf)))
            (start-server :usocket #(0 0 0 0) 7002 #(127 0 0 1) swank-port)))
        (delete-file swank-port-file))
      (:fd-handler
        (asdf:load-system :vlime-sbcl)
        (swank:start-server swank-port-file :style preferred-style :dont-close t)
        (with-open-file (pf swank-port-file)
          (let* ((swank-port (read pf)))
            (start-server :sbcl #(0 0 0 0) 7002 #(127 0 0 1) swank-port)))
        (delete-file swank-port-file))
      ;((nil)) ; TODO
      (t
        (vom:error "Communication style ~s not supported." preferred-style)))))
