(in-package #:cl-user)

(defpackage #:vlime-usocket
  (:use #:cl
        #:usocket
        #:vlime-protocl)
  (:export #:main))

(in-package #:vlime-usocket)


(defun server-listener (socket swank-host swank-port)
  (vom:debug "Server created: ~s" socket)
  (loop
    (handler-case
      (let ((client-socket (socket-accept socket)))
        (swank/backend:spawn
          #'(lambda ()
              (vlime-control-thread
                client-socket swank-host swank-port))
          :name (format nil "Vlime Control Thread" swank-host swank-port)))
      (t (c)
         (vom:error "server-listener: ~a" c)
         (socket-close socket)
         (vom:error "Listener socket stopped." c)
         (return-from server-listener)))))


(defun vlime-control-thread (client-socket swank-host swank-port)
  (vom:debug "New client: ~s" client-socket)

  (let* ((control-thread (swank/backend:current-thread))
         (swank-socket
           (handler-case
             (socket-connect swank-host swank-port
                             :protocol :stream
                             :element-type '(unsigned-byte 8))
             (t (c)
                (vom:error "Failed to connect to SWANK: ~s ~s: ~a"
                           swank-host swank-port c)
                (socket-close client-socket)
                (return-from vlime-control-thread))))
         (client-stream (socket-stream client-socket))
         (swank-stream (socket-stream swank-socket)))

    (labels ((client-read-loop ()
               (let ((line
                       (handler-case
                         (read-line client-stream)
                         (t (c)
                            (swank/backend:send control-thread `(:client-eof ,c))
                            nil))))
                 (when line
                   (swank/backend:send control-thread `(:client-data ,line))
                   (client-read-loop))))

             (read-swank-data (buf)
               (handler-case
                 (let ((read-len (read-sequence buf swank-stream)))
                   (if (< read-len (length buf))
                     (progn
                       (swank/backend:send control-thread `(:swank-eof))
                       nil)
                     read-len))
                 (t (c)
                    (swank/backend:send control-thread `(:swank-eof ,c))
                    nil)))

             (swank-read-loop (&optional (msg-len-buf nil))
               (when (not msg-len-buf)
                 (setf msg-len-buf
                       (make-array
                         +swank-msg-len-size+
                         :element-type '(unsigned-byte 8))))
               (when (read-swank-data msg-len-buf)
                 (let* ((msg-len (handler-case
                                   (parse-swank-msg-len msg-len-buf)
                                   (t (c)
                                      (swank/backend:send control-thread `(:swank-eof ,c))
                                      (return-from swank-read-loop))))
                        (msg-buf (handler-case
                                   (make-array msg-len :element-type '(unsigned-byte 8))
                                   (t (c)
                                      (swank/backend:send control-thread `(:swank-eof ,c))
                                      (return-from swank-read-loop)))))
                   (when (read-swank-data msg-buf)
                     (swank/backend:send control-thread `(:swank-data ,msg-buf))
                     (swank-read-loop msg-len-buf))))))

      (let ((client-read-thread (swank/backend:spawn
                                  #'client-read-loop
                                  :name "Vlime Client Reader"))
            (swank-read-thread (swank/backend:spawn
                                 #'swank-read-loop
                                 :name "Vlime SWANK Reader")))
        (loop
          (let ((msg (swank/backend:receive)))
            (ecase (car msg)
              (:client-data
                (vom:debug "client-data msg")
                (handler-case
                  (let ((line (nth 1 msg)))
                    (vom:debug "Message from client: ~s" line)
                    (write-sequence (msg-client-to-swank line :octets)
                                    swank-stream)
                    (finish-output swank-stream))
                  (t (c)
                     (swank/backend:send control-thread `(:client-data-error ,c)))))

              (:swank-data
                (vom:debug "swank-data msg")
                (handler-case
                  (let ((swank-msg (swank/backend:utf8-to-string (nth 1 msg))))
                    (vom:debug "Message from SWANK: ~s" swank-msg)
                    (write-sequence (msg-swank-to-client swank-msg :string)
                                    client-stream)
                    (finish-output client-stream))
                  (t (c)
                     (swank/backend:send control-thread `(:swank-data-error ,c)))))

              ((:exit :client-eof :swank-eof :client-data-error :swank-data-error)
                (vom:debug "Control thread stopping: ~s" msg)
                (swank/backend:kill-thread swank-read-thread)
                (swank/backend:kill-thread client-read-thread)
                (socket-close swank-socket)
                (socket-close client-socket)
                (return-from vlime-control-thread)))))))))


(defun main (host port swank-host swank-port)
  (vom:config t :debug)
  (let ((server-socket
          (socket-listen host port
                         :reuse-address t
                         :backlog 128
                         :element-type 'character)))
    (swank/backend:spawn
      #'(lambda ()
          (server-listener server-socket swank-host swank-port))
      :name (format nil "Vlime Server Listener ~a ~a" host port))))
