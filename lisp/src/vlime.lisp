(in-package #:cl-user)

(defpackage #:vlime
  (:use #:cl
        #:cl-async
        #:vlime-protocl)
  (:export #:main))

(in-package #:vlime)


(defvar *connections* nil)

(defparameter +default-buffer-size+ 4096)
(defparameter +swank-msg-len-size+ 6)


(defclass connection ()
  ((socket
     :accessor connection-socket
     :initarg :socket
     :initform nil)
   (read-buffer
     :accessor connection-read-buffer
     :initarg :read-buffer
     :initform (vector))
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
   (with-slots (socket peer) connection
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


(defun parse-line (data conn)
  (with-slots (read-buffer) conn
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
        else return lines))))


(defun parse-swank-msg-len (buffer)
  (parse-integer (babel:octets-to-string (subseq buffer 0 +swank-msg-len-size+)) :radix 16))


(defun parse-swank-msg (data conn)
  (with-slots (read-buffer) conn
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
          return msgs))))


(defun handle-swank-msg (msg swank-conn)
  (let ((msg-to-client (msg-swank-to-client msg)))
    (with-slots ((client-conn peer)) swank-conn
      (as:write-socket-data
        (connection-socket client-conn)
        (babel:string-to-octets msg-to-client)))))


(defun swank-read-cb (socket data)
  (let* ((swank-conn (gethash socket *connections*))
         (msg-list (parse-swank-msg data swank-conn)))
    (when msg-list
      (dolist (msg msg-list)
        (vom:debug "Message from SWANK: ~s" msg)
        (handler-case (handle-swank-msg msg swank-conn)
          (error (err)
                 (vom:debug
                   "Failed to handle SWANK message: ~s~a: ~s"
                   (if (> (length msg) 32) (subseq msg 0 32) msg)
                   (if (> (length msg) 32) "..." "")
                   err)))))))


(defun handle-client-msg (msg client-conn)
  (let ((msg-to-swank (msg-client-to-swank msg)))
    (with-slots ((swank-conn peer)) client-conn
      (as:write-socket-data
        (connection-socket swank-conn)
        (babel:string-to-octets msg-to-swank)))))


(defun ensure-swank-connection (client-conn swank-host swank-port)
  (with-slots ((swank-conn peer)) client-conn
    (when (not swank-conn)
      (vom:debug "Connecting to peer...")
      (let ((swank-socket
              (tcp-connect swank-host swank-port
                           #'swank-read-cb
                           :event-cb  #'socket-event-cb)))
        (make-connection :socket swank-socket :peer client-conn)
        (vom:debug "Connection count: ~s" (hash-table-count *connections*))))))


(defun client-read-cb (socket data swank-host swank-port)
  (let* ((client-conn (gethash socket *connections*))
         (line-list (parse-line data client-conn)))
    (when line-list
      (ensure-swank-connection client-conn swank-host swank-port)
      (dolist (line line-list)
        (vom:debug "Message from ~s: ~s" socket line)
        (handle-client-msg line client-conn)))))


(defun main (host port swank-host swank-port)
  (vom:config t :debug)
  (let ((*connections* (make-hash-table)))
    (with-event-loop (:catch-app-errors t)
      (let ((server (as::tcp-server-new
                      host port
                      #'(lambda (socket data)
                          (client-read-cb socket data
                                          swank-host swank-port))
                      :event-cb #'socket-event-cb
                      :connect-cb #'client-connect-cb)))
        (vom:debug "Server created: ~s" server)))))
