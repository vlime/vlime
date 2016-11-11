(require :asdf)

(defparameter *vlime-home* (make-pathname :directory (pathname-directory *load-truename*)))

(defun load-vlime ()
  (asdf/find-system:initialize-source-registry
    `(:source-registry
       (:directory ,*vlime-home*)
       :inherit-configuration))
  (asdf/operate:load-system :vlime))

(load-vlime)
(swank:create-server :port 4005 :style :spawn :dont-close t)
(vlime:main)
