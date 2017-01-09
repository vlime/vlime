(in-package #:cl-user)


(defpackage #:vlime-protocl
  (:use #:cl)
  (:export #:parse-form
           #:seq-client-to-swank
           #:seq-swank-to-client
           #:client-emacs-rex-p
           #:remove-client-seq
           #:form-to-json
           #:json-to-form
           #:msg-client-to-swank
           #:msg-swank-to-client))


(defpackage #:vlime
  (:use #:cl
        #:cl-async
        #:vlime-protocl)
  (:export #:main))
