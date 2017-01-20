(in-package #:cl-user)

(defpackage #:vlime-protocl
  (:use #:cl)
  (:export #:+swank-msg-len-size+
           #:parse-form
           #:seq-client-to-swank
           #:seq-swank-to-client
           #:client-emacs-rex-p
           #:remove-client-seq
           #:form-to-json
           #:json-to-form
           #:msg-client-to-swank
           #:msg-swank-to-client
           #:parse-line
           #:parse-swank-msg-len
           #:parse-swank-msg))

(in-package #:vlime-protocl)


(defparameter +swank-msg-len-size+ 6)


(defun parse-form (input-str)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string input-str))))


(defun seq-client-to-swank (form)
  (let ((seq (car form))
        (payload (cadr form)))
    (concatenate 'list payload (list seq))))


(defun seq-swank-to-client (form)
  (let ((seq (car (last form)))
        (payload (subseq form 0 (1- (length form)))))
    (list seq payload)))


(defun client-emacs-rex-p (form)
  (and (listp form)
       (listp (nth 1 form))
       (eql (car (nth 1 form)) :emacs-rex)))


(defun remove-client-seq (form)
  (nth 1 form))


(defun form-to-json (form)
  (cond
    ((listp form)
     (mapcar #'form-to-json form))
    ((eql form t)
     ; special case to prevent T from being serialized as a normal symbol,
     ; thus saving some space
     form)
    ((symbolp form)
     (let ((sym-obj (make-hash-table :test #'equal))
           (sym-name (symbol-name form))
           (sym-package (package-name (symbol-package form))))
       (setf (gethash "name" sym-obj) sym-name)
       (setf (gethash "package" sym-obj) sym-package)
       sym-obj))
    (t
     ; Numbers & strings
     form)))


(defun json-to-form (json)
  (cond
    ((listp json)
     (mapcar #'json-to-form json))
    ((hash-table-p json)
     (let ((sym-name (gethash "name" json))
           (sym-package (gethash "package" json)))
       (intern sym-name sym-package)))
    (t
     ; Numbers & strings
     json)))


(defun msg-client-to-swank (msg)
  (let* ((json (yason:parse msg))
         (form (json-to-form json)))
    (if (client-emacs-rex-p form)
      (let* ((emacs-rex-form (seq-client-to-swank form))
             (form-str (with-standard-io-syntax (write-to-string emacs-rex-form)))
             (form-str-len (length form-str)))
        (format nil "~6,'0x~a" form-str-len form-str))
      (let* ((one-way-form (remove-client-seq form))
             (form-str (with-standard-io-syntax (write-to-string one-way-form)))
             (form-str-len (length form-str)))
        (format nil "~6,'0x~a" form-str-len form-str)))))


(defun msg-swank-to-client (msg)
  (let* ((form (parse-form msg))
         (msg-type (car form)))
    (if (eql msg-type :return)
      (let* ((json (seq-swank-to-client (form-to-json form)))
             (encoded (with-output-to-string (json-out)
                        (yason:encode json json-out))))
        (concatenate 'string encoded (format nil "~a" #\newline)))
      (let* ((json (form-to-json form))
             (active-msg (list 0 json))
             (encoded (with-output-to-string (json-out)
                        (yason:encode active-msg json-out))))
        (concatenate 'string encoded (format nil "~a" #\newline))))))


(defun parse-line (data read-buffer)
  (setf read-buffer
        (concatenate '(vector (unsigned-byte 8)) read-buffer data))
  (let ((lf-pos (position (char-code #\linefeed) read-buffer)))
    (loop
      when lf-pos
        collect (babel:octets-to-string (subseq read-buffer 0 (1+ lf-pos))) into lines
        and do
          (setf read-buffer (subseq read-buffer (1+ lf-pos)))
          (setf lf-pos
                (position (char-code #\linefeed) read-buffer :start 0))
      else
        return (values lines read-buffer))))


(defun parse-swank-msg-len (buffer)
  (parse-integer
    (babel:octets-to-string (subseq buffer 0 +swank-msg-len-size+))
    :radix 16))


(defun parse-swank-msg (data read-buffer)
  (setf read-buffer
        (concatenate '(vector (unsigned-byte 8)) read-buffer data))
  (let ((msg-total-len
          (when (>= (length read-buffer) +swank-msg-len-size+)
            (+ +swank-msg-len-size+
               (parse-swank-msg-len read-buffer)))))
    (loop
      when (and msg-total-len (>= (length read-buffer) msg-total-len))
        collect (babel:octets-to-string
                  (subseq read-buffer +swank-msg-len-size+ msg-total-len))
                into msgs
        and do
          (setf read-buffer (subseq read-buffer msg-total-len))
          (if (>= (length read-buffer) +swank-msg-len-size+)
            (setf msg-total-len
                  (+ +swank-msg-len-size+
                     (parse-swank-msg-len read-buffer)))
            (setf msg-total-len nil))
      else
        return (values msgs read-buffer))))
