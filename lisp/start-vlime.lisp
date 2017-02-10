(require :asdf)

(defparameter *vlime-home* (make-pathname :directory (pathname-directory *load-truename*)))

(defun load-vlime ()
  (asdf:initialize-source-registry
    `(:source-registry
       (:directory ,*vlime-home*)
       :inherit-configuration))
  (asdf:load-system :vlime))

(load-vlime)
(vlime:main *vlime-home*)
