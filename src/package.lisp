(in-package #:cl-user)


(defpackage #:vlime
  (:use #:cl
        #:cl-async
        #:blackbird
        #:lparallel)
  (:shadowing-import-from #:cl-async #:delay)
  (:shadowing-import-from #:blackbird #:chain #:promise)
  (:export #:main))
