(require :asdf)

(defparameter *vlime-home* (make-pathname :directory (pathname-directory *load-truename*)))

(defun load-vlime ()
  (asdf:initialize-source-registry
    `(:source-registry
       (:directory ,*vlime-home*)
       :inherit-configuration))
  (asdf:load-system :vlime-usocket))

(load-vlime)
(swank:create-server :port 4005 :style :spawn :dont-close t)
(vlime-usocket:main #(0 0 0 0) 7002 #(127 0 0 1) 4005)
