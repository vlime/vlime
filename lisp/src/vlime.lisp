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


(defgeneric connection-close (connection)
  (:method ((connection connection))
   (with-slots (socket stream peer) connection
     (when stream (close stream))
     (when peer
       (vom:debug "Closing peer connection...")
       (setf (connection-peer peer) nil)
       (connection-close peer))
     (remhash socket *connections*))))


(defun socket-event-cb (event)
  (handler-case (error event)
    (socket-eof ()
      (let* ((socket (socket event))
             (conn (gethash socket *connections*)))
        (vom:debug "socket-closed: ~s" (socket-closed-p socket))
        (connection-close conn)
        (vom:debug "Connection count: ~s" (hash-table-count *connections*))))
    (error ()
      (vom:debug "Socket event: ~a" event))))


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


(defun handle-swank-msg (msg swank-conn)
  (let* ((msg-str (babel:octets-to-string msg))
         (msg-to-client (msg-swank-to-client msg-str)))
    (with-slots ((client-conn peer)) swank-conn
      (write-sequence
        (babel:string-to-octets msg-to-client)
        (connection-stream client-conn)))))


(defun swank-read-cb (socket stream)
  (let* ((data-cache '())
         (swank-conn (gethash socket *connections*))
         (msg-list (async-stream-read-swank-msg stream swank-conn data-cache)))
    (when msg-list
      ;(vom:debug "Raw data from SWANK ~s: ~s" socket msg-list)
      (dolist (msg msg-list)
        (vom:debug "Message from SWANK: ~s" (babel:octets-to-string msg))
        (handler-case (handle-swank-msg msg swank-conn)
          (error (err)
                 (vom:debug
                   "Failed to handle SWANK message: ~s~a: ~s"
                   (if (> (length msg) 32) (subseq msg 0 32) msg)
                   (if (> (length msg) 32) "..." "")
                   err)))))))


(defun handle-client-msg (msg client-conn)
  (let* ((msg-str (babel:octets-to-string msg))
         (msg-to-swank (msg-client-to-swank msg-str)))
    (with-slots ((swank-conn peer)) client-conn
      (write-sequence
        (babel:string-to-octets msg-to-swank)
        (connection-stream swank-conn)))))


(defun ensure-swank-connection (client-conn swank-host swank-port)
  (with-slots ((swank-conn peer)) client-conn
    (when (not swank-conn)
      (vom:debug "Connecting to peer...")
      (let* ((swank-stream
               (tcp-connect swank-host swank-port
                            #'swank-read-cb
                            :event-cb  #'socket-event-cb
                            :stream t))
             (swank-socket (streamish swank-stream)))
        (make-connection :socket swank-socket
                         :stream swank-stream
                         :peer client-conn)
        (vom:debug "Connection count: ~s" (hash-table-count *connections*))))))


(defun client-read-cb (socket stream swank-host swank-port)
  (let* ((data-cache '())
         (client-conn (gethash socket *connections*))
         (line-list (async-stream-read-line stream client-conn data-cache)))
    (when line-list
      (with-slots ((client-stream stream)) client-conn
        (when (not client-stream)
          (setf client-stream stream)))
      (ensure-swank-connection client-conn swank-host swank-port)
      (vom:debug "Raw data from ~s: ~s" socket line-list)
      (dolist (line line-list)
        (handle-client-msg line client-conn)))))


(defun main (host port swank-host swank-port)
  (vom:config t :debug)
  (let ((*connections* (make-hash-table)))
    (with-event-loop (:catch-app-errors t)
      (let ((server (as::tcp-server-new
                      host port
                      #'(lambda (socket stream)
                          (client-read-cb socket stream
                                          swank-host swank-port))
                      :event-cb #'socket-event-cb
                      :connect-cb #'client-connect-cb
                      :stream t)))
        (vom:debug "Server created: ~s" server)))))
