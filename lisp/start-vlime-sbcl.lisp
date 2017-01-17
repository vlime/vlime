(require :asdf)

(defparameter *vlime-home* (make-pathname :directory (pathname-directory *load-truename*)))

(defun load-vlime ()
  (asdf/find-system:initialize-source-registry
    `(:source-registry
       (:directory ,*vlime-home*)
       :inherit-configuration))
  (asdf/operate:load-system :vlime-sbcl))

(load-vlime)
(swank:create-server :port 4005 :style :spawn :dont-close t)
#-windows
(vlime-sbcl:main #(0 0 0 0) 7002 #(127 0 0 1) 4005)
#+windows
(vlime-sbcl:main-threaded #(0 0 0 0) 7002 #(127 0 0 1) 4005)
