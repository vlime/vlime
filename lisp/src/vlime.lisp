(in-package #:cl-user)

(defpackage #:vlime
  (:use #:cl
        #:cl-async
        #:vlime-protocl
        #:vlime-connection)
  (:export #:main))

(in-package #:vlime)


(defun socket-event-cb (event)
  (handler-case (error event)
    (socket-eof ()
      (let* ((socket (socket event))
             (conn (lookup-connection socket)))
        (vom:debug "socket-closed: ~s" (socket-closed-p socket))
        (connection-close conn)
        (vom:debug "Connection count: ~s" (count-connections))))
    (error ()
      (vom:debug "Socket event: ~a" event))))


(defun client-connect-cb (socket)
  (unless (lookup-connection socket)
    (make-connection :socket socket)
    (vom:debug "New connection from ~s" socket)
    (vom:debug "Connection count: ~s" (count-connections))))


(defun swank-read-cb (socket data)
  (let ((swank-conn (lookup-connection socket)))
    (multiple-value-bind (msg-list buffered)
                         (parse-swank-msg
                           data (connection-read-buffer swank-conn))
      (setf (connection-read-buffer swank-conn) buffered)
      (when msg-list
        (dolist (msg msg-list)
          (vom:debug "Message from SWANK: ~s" msg)
          (handler-case
            (as:write-socket-data
              (connection-socket (connection-peer swank-conn))
              (babel:string-to-octets (msg-swank-to-client msg)))
            (error (err)
                   (vom:debug
                     "Failed to handle SWANK message: ~s~a: ~s"
                     (if (> (length msg) 32) (subseq msg 0 32) msg)
                     (if (> (length msg) 32) "..." "")
                     err))))))))


(defun client-read-cb (socket data swank-host swank-port)
  (let ((client-conn (lookup-connection socket)))
    (multiple-value-bind (line-list buffered)
                         (parse-line
                           data (connection-read-buffer client-conn))
      (setf (connection-read-buffer client-conn) buffered)
      (when line-list
        (ensure-peer-connection
          client-conn
          #'(lambda ()
              (tcp-connect swank-host swank-port
                           #'swank-read-cb
                           :event-cb #'socket-event-cb)))
        (dolist (line line-list)
          (vom:debug "Message from ~s: ~s" socket line)
          (as:write-socket-data
            (connection-socket (connection-peer client-conn))
            (babel:string-to-octets (msg-client-to-swank line))))))))


(defun main (host port swank-host swank-port)
  (vom:config t :debug)
  (let ((vlime-connection:*connections* (make-hash-table)))
    (with-event-loop (:catch-app-errors t)
      (let ((server (as::tcp-server-new
                      host port
                      #'(lambda (socket data)
                          (client-read-cb socket data
                                          swank-host swank-port))
                      :event-cb #'socket-event-cb
                      :connect-cb #'client-connect-cb)))
        (vom:debug "Server created: ~s" server)))))
