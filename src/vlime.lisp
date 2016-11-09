(in-package #:vlime)


(defvar *connections* nil)

(defparameter +default-buffer-size+ 4096)
(defparameter +swank-msg-len-size+ 6)


(defun make-buffer ()
  (make-array
    +default-buffer-size+
    :element-type '(unsigned-byte 8)
    :initial-element 0))


(defclass connection ()
  ((socket
     :accessor connection-socket
     :initarg :socket
     :initform nil)
   (stream
     :accessor connection-stream
     :initarg :stream
     :initform nil)
   (read-buffer
     :accessor connection-read-buffer
     :initarg :read-buffer
     :initform (make-buffer))
   (read-buffer-filled
     :accessor connection-read-buffer-filled
     :initarg :read-buffer-filled
     :initform 0)
   (peer
     :accessor connection-peer
     :initarg :peer
     :initform nil)))


(defun make-connection (&rest args)
  (let ((new-conn (apply #'make-instance 'connection args))
        (socket (getf args :socket))
        (peer (getf args :peer)))
    (when peer
      (setf (connection-peer peer) new-conn))
    (when socket
      (setf (gethash socket *connections*) new-conn))
    new-conn))


(defgeneric connection-close (connection))

(defmethod connection-close ((connection connection))
  (with-slots (socket stream peer) connection
    (when stream (close stream))
    (when peer
      (vom:debug "Closing peer connection...")
      (setf (connection-peer peer) nil)
      (connection-close peer))
    (remhash socket *connections*)))


(defun socket-event-cb (event)
  (handler-case (error event)
    (socket-eof ()
      (let* ((socket (socket event))
             (conn (gethash socket *connections*)))
        (vom:debug "socket-closed: ~s" (socket-closed-p socket))
        (connection-close conn)
        (vom:debug "Connection count: ~s" (hash-table-count *connections*))))
    (error ()
      (vom:debug "Socket event: ~s" event))))


(defun client-connect-cb (socket)
  (unless (nth-value 1 (gethash socket *connections*))
    (make-connection :socket socket)
    (vom:debug "New connection from ~s" socket)
    (vom:debug "Connection count: ~s" (hash-table-count *connections*))))


(defun pop-buffer-data (conn len)
  (with-slots (read-buffer read-buffer-filled) conn
    (let ((ret (subseq read-buffer 0 len)))
      (decf read-buffer-filled len)
      (dotimes (i read-buffer-filled)
        (setf (aref read-buffer i) (aref read-buffer (+ len i))))
      ret)))


(defun async-stream-read-line (stream conn data-list)
  (with-slots (read-buffer read-buffer-filled) conn
    (let* ((read-len (read-sequence read-buffer stream :start read-buffer-filled))
           (new-filled (+ read-len read-buffer-filled))
           (lf-pos (position (char-code #\linefeed)
                             read-buffer
                             :start read-buffer-filled
                             :end new-filled)))
      (cond
        (lf-pos
         (setf read-buffer-filled new-filled)
         (loop
           when lf-pos
             do
               (push (pop-buffer-data conn (1+ lf-pos)) data-list)
               (setf lf-pos
                     (position (char-code #\linefeed)
                               read-buffer
                               :start 0
                               :end read-buffer-filled))
             and collect (apply #'concatenate
                                '(vector (unsigned-byte 8))
                                (reverse data-list)) into lines
             and do (setf data-list nil)
           else return lines))
        ((>= new-filled (length read-buffer))
         (push read-buffer data-list)
         (setf read-buffer-filled 0)
         (setf read-buffer (make-buffer))
         (async-stream-read-line stream conn data-list))
        (t
         (let ((data-len (apply #'+ (mapcar #'length data-list))))
           (push read-buffer data-list)
           (setf read-buffer-filled (+ data-len new-filled))
           (setf read-buffer
                 (apply #'concatenate
                        '(vector (unsigned-byte 8))
                        (reverse data-list))))
         nil)))))


(defun parse-swank-msg-len (buffer)
  (parse-integer (babel:octets-to-string (subseq buffer 0 +swank-msg-len-size+)) :radix 16))


(defun async-stream-read-swank-msg (stream conn data-list)
  (with-slots (read-buffer read-buffer-filled) conn
    (let* ((read-len (read-sequence read-buffer stream :start read-buffer-filled))
           (new-filled (+ read-len read-buffer-filled))
           (msg-total-len
             (cond
               (data-list
                (+ +swank-msg-len-size+ (parse-swank-msg-len (car (last data-list)))))
               ((>= new-filled +swank-msg-len-size+)
                (+ +swank-msg-len-size+ (parse-swank-msg-len read-buffer)))
               (t
                nil)))
           (data-len (apply #'+ (mapcar #'length data-list))))
      (cond
        ((and msg-total-len (<= msg-total-len (+ data-len new-filled)))
         (push read-buffer data-list)
         (setf read-buffer-filled (+ data-len new-filled))
         (setf read-buffer (apply #'concatenate
                                  '(vector (unsigned-byte 8))
                                  (reverse data-list)))
         (loop
           when (and msg-total-len (<= msg-total-len read-buffer-filled))
             collect (subseq (pop-buffer-data conn msg-total-len)
                             +swank-msg-len-size+) into msgs
             and do
               (if (>= read-buffer-filled +swank-msg-len-size+)
                 (setf msg-total-len
                       (+ +swank-msg-len-size+ (parse-swank-msg-len read-buffer)))
                 (setf msg-total-len nil))
           else
             return msgs))
        ((and msg-total-len (>= new-filled (length read-buffer))) ; and  (< (+ data-len new-filled) msg-total-len)
         (push read-buffer data-list)
         (setf read-buffer-filled 0)
         (setf read-buffer (make-buffer))
         (async-stream-read-swank-msg stream conn data-list))
        (t
         (push read-buffer data-list)
         (setf read-buffer-filled (+ data-len new-filled))
         (setf read-buffer (apply #'concatenate
                                  '(vector (unsigned-byte 8))
                                  (reverse data-list)))
         nil)))))


(defun parse-forms (input-str)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string input-str))))


(defun swank-read-cb (socket stream)
  (let* ((data-cache '())
         (swank-conn (gethash socket *connections*))
         (msg-list (async-stream-read-swank-msg stream swank-conn data-cache)))
    (when msg-list
      ;(vom:debug "Raw data from SWANK ~s: ~s" socket msg-list)
      (dolist (msg msg-list)
        (vom:debug "Message from SWANK: ~s" (babel:octets-to-string msg))
        (let ((encoded (form-to-json (parse-forms (babel:octets-to-string msg)))))
          (with-slots ((client-conn peer)) swank-conn
            (write-sequence (babel:string-to-octets encoded)
                            (connection-stream client-conn))
            (write-sequence (babel:string-to-octets (format nil "~a" #\newline))
                            (connection-stream client-conn))))))))


(defun client-read-cb (socket stream)
  (let* ((data-cache '())
         (client-conn (gethash socket *connections*))
         (line-list (async-stream-read-line stream client-conn data-cache)))
    (when line-list
      (with-slots ((client-stream stream) (swank-conn peer)) client-conn
        (when (not client-stream)
          (setf client-stream stream))
        (when (not swank-conn)
          (vom:debug "Connecting to SWANK...")
          (let* ((swank-stream
                   (tcp-connect "127.0.0.1" 4005
                                #'swank-read-cb
                                :event-cb  #'socket-event-cb
                                :stream t))
                 (swank-socket (streamish swank-stream)))
            (make-connection :socket swank-socket
                             :stream swank-stream
                             :peer client-conn)
            (vom:debug "Connection count: ~s" (hash-table-count *connections*))))
        (vom:debug "Raw data from ~s: ~s" socket line-list)
        (loop for line in line-list
              do (let* ((line-str (babel:octets-to-string line))
                        (json (handler-case (yason:parse line-str)
                                (error (co)
                                       (vom:debug "Bad JSON data (~s): ~s" co line-str)
                                       nil))))
                   (vom:debug "Data from ~s: ~s" socket json)
                   (when json
                     ; TODO: json-to-form
                     (write-sequence
                       (babel:string-to-octets
                         "00002c(:emacs-rex (swank:connection-info) nil t 5)") (connection-stream swank-conn)))))))))


(defun form-to-json (form)
  (case (car form)
    (:emacs-rex
      (let ((res (make-hash-table :test #'equal)))
        (setf (gethash "type" res) "emacs-rex")
        (setf (gethash "form" res)
              (with-standard-io-syntax
                (nth 1 form)))
        (setf (gethash "seq" res) (nth 2 form))
        (with-output-to-string (json-out)
          (yason:encode res json-out))))
    (:return
      (let ((res (make-hash-table :test #'equal)))
        (setf (gethash "type" res) "return")
        (setf (gethash "form" res)
              (with-standard-io-syntax
                (write-to-string (nth 1 form))))
        (setf (gethash "seq" res) (nth 2 form))
        (with-output-to-string (json-out)
          (yason:encode res json-out))))
    (t
      (error "Unknown form: ~s" form))))


(defun main ()
  (vom:config t :debug)
  (let ((*connections* (make-hash-table)))
    (with-event-loop (:catch-app-errors t)
      (let ((server (as::tcp-server-new
                      "127.0.0.1"
                      7002
                      #'client-read-cb
                      :event-cb #'socket-event-cb
                      :connect-cb #'client-connect-cb
                      :stream t)))
        (vom:debug "Server created: ~s" server)))))
