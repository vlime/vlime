(in-package :vlime)


(defvar vlime-symbol-prefix "§§")


(defmethod yason:encode ((sym symbol) &optional (stream yason::*json-output*))
  (yason:encode
    (yason::encode-symbol-as-string sym nil vlime-symbol-prefix)
    stream))

(defun encode-via-yason (obj package)
  (declare (ignore package))
  (with-output-to-string (s) 
    (let ((yason:*list-encoder* #'yason:encode-plain-list-to-array)
          (yason:*symbol-key-encoder* 
            (lambda (sym)
              (yason:encode-symbol-as-string sym nil 
                                             vlime-symbol-prefix))))
      (yason:encode obj s))))

(defun recover-symbols (input &optional package)
  (declare (ignore package))
  (labels 
      ((make-sym (stg)
         (let* ((p (position #\: stg))
                (later-colon (if (eql #\: 
                                      (aref stg (1+ p)))
                                 (1+ p)
                                 p)))
           ;; FIND-SYMBOL or INTERN?
           (cond
             ((zerop p)
              (intern (subseq stg (1+ later-colon))
                      :keyword))
             (T
              ;; get PKG::SYM
              (intern (subseq stg (+ 2 p))
                      (subseq stg 0 p))))))
       (rec (tree)
         (cond 
           ((null tree)
            tree)
           ((stringp tree)
            (multiple-value-bind (match? after)
                (alexandria:starts-with-subseq vlime-symbol-prefix 
                                               tree
                                               :test #'eql
                                               :return-suffix t)
              (if match?
                  (make-sym after)
                  tree)))
           ((consp tree)
            (cons (rec (car tree))
                  (rec (cdr tree))))
           ((arrayp tree)
            (map 'vector #'rec tree))
           (t
            tree))))
    (let ((tree0 (yason:parse input)))
      (rec tree0))))

(defun use-json ()
  (swank::set-data-protocol
    #'encode-via-yason
    #'recover-symbols))
