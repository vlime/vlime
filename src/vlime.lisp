(in-package #:vlime)


(defvar *connections* nil)

(defparameter +default-buffer-size+ 4096)


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
   (read-buffer
     :accessor connection-read-buffer
     :initarg :read-buffer
     :initform (make-buffer))
   (read-buffer-filled
     :accessor connection-read-buffer-filled
     :initarg :read-buffer-filled
     :initform 0)))


(defun server-connect-cb (socket)
  (unless (nth-value 1 (gethash socket *connections*))
    (let ((conn (make-instance 'connection :socket socket)))
      (setf (gethash socket *connections*) conn)
      (vom:debug "New connection from ~s" socket))))


(defun async-stream-read-line (stream conn data-list)
  (let* ((read-buffer (connection-read-buffer conn))
         (filled (connection-read-buffer-filled conn))
         (read-len (read-sequence read-buffer stream :start filled))
         (new-filled (+ read-len filled))
         (lf-pos (position (char-code #\linefeed)
                           read-buffer
                           :start filled
                           :end new-filled)))
    (cond
      (lf-pos
        (push (subseq read-buffer 0 lf-pos) data-list)
        (setf (connection-read-buffer-filled conn) (- new-filled lf-pos))
        (dotimes (i (connection-read-buffer-filled conn))
          (setf (aref read-buffer i) (aref read-buffer (+ lf-pos i))))
        (babel:octets-to-string
          (apply #'concatenate
                 '(vector (unsigned-byte 8))
                 (reverse data-list))))
      ((>= new-filled (length read-buffer))
        (setf (connection-read-buffer-filled conn) 0)
        (setf (connection-read-buffer conn) (make-buffer))
        (push read-buffer data-list)
        (async-stream-read-line stream conn data-list))
      (t
        (let ((data-len (apply #'+ (mapcar #'length data-list))))
          (push read-buffer data-list)
          (setf (connection-read-buffer-filled conn)
                (+ data-len new-filled))
          (setf (connection-read-buffer conn)
                (apply #'concatenate
                       '(vector (unsigned-byte 8))
                       (reverse data-list))))
        nil))))


(defun server-read-cb (socket stream)
  (let* ((data-cache '())
         (conn (gethash socket *connections*))
         (line (async-stream-read-line stream conn data-cache)))
    (when line
      (vom:debug "Raw data from ~s: ~s" socket line)
      (let ((json (yason:parse line)))
        (vom:debug "Data from ~s: ~s" socket json)
        (let ((encoded
                (with-output-to-string (json-out)
                  (yason:encode json json-out))))
          (write-sequence (babel:string-to-octets encoded) stream)
          (write-sequence (babel:string-to-octets
                            (format nil "~a" #\newline)) stream))))))


(defun server-event-cb (event)
  (handler-case (error event)
    (socket-eof ()
      (let ((socket (socket event)))
        (vom:debug "socket-closed: ~s" (socket-closed-p socket))
        (remhash socket *connections*)))
    (error ()
      (vom:debug "Socket event: ~s" event))))


(defun main ()
  (vom:config t :debug)
  (let ((*connections* (make-hash-table)))
    (with-event-loop (:catch-app-errors t)
      (let ((server (as::tcp-server-new
                      "127.0.0.1"
                      7002
                      #'server-read-cb
                      :event-cb #'server-event-cb
                      :connect-cb #'server-connect-cb
                      :stream t)))
        (vom:debug "Server created: ~s" server)))))
