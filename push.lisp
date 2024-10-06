(push "/home/john/hal9k/" ql:*local-project-directories*)

(ql:quickload "drakma")
(ql:quickload "local-time")
(ql:quickload "cl-json")
(ql:quickload "cl-mqtt")

;; get'em debug frames
;; (declaim (optimize (debug 3)))

(defun topic->object-name (topic)
  (let ((start (search "-" topic))
        (end (or (search "/get" topic)
                 (search "/set" topic))))
    (if (not start)
        topic
        (subseq topic (+ 1 start) end))))

(defun jv (json-string key)
  "Returns the value pointed to by `KEY' in `JSON-STRING'. Use like `GETF'."
  (let ((decoded (json:decode-json-from-string json-string)))
    (loop for el in decoded do
          (if (equal (car el) key) (return (cdr el))))))

(defun log-temperature (topic payload)
  "Log a temperature sensor value"
  (let ((name (topic->object-name topic))
        (temp (jv payload :temperature)))

    (unless temp
      (format t "Unknown format: [~A] ~A~%"
              topic payload)
      (return-from log-temperature nil))

    ;; (push-data-to-influxdb temp name :dry t)
    (push-data-to-influxdb temp name)))

(defun app-handle-topic (topic payload)
  "Find and call a message handler for a given topic"
  (when (search "z2m/temp" topic)

    ;; Assume utf-8 strings
    (setf payload (mqtt:ascii->string payload))

    (format t "handling: [~A] ~A~%" topic payload)
    (log-temperature topic payload)))

(defun app-process-packet (parsed)
  (if parsed
      (case (first parsed)
        (:pingrsp nil)
        (:publish (app-handle-topic
                   (getf (cdr parsed) :topic)
                   (getf (cdr parsed) :payload)))
        (t (format t "Got packet[~A]: ~X~%" (length parsed) parsed)))))

(defun app-callback (broker data)
  (setf *broker* broker)
  (when (> (length data) 0)
    (mapcar #'app-process-packet
            (mqtt:parse-packets (coerce data 'list)))))

(defun main ()
  (handler-case (mqtt:connect-to-broker "192.168.10.175" 1883 #'app-callback)
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (format *error-output* "Caught interrupt, aborting~%")
           (uiop:quit)))
    (error (c) (format t "Unknown error occured:~&~a~&" c))))

(defun push-data-to-influxdb (temp channel &key dry)
  (let* ((influxdb-host "192.168.10.175")
         (influxdb-port 8086)
         (influxdb-org "org")
         (influxdb-bucket "series")
         (influxdb-token "homesweethome")
         (timestamp (format nil "~D" (* 1000 (local-time:timestamp-to-unix (local-time:now)))))
         (data (format nil "temperature,channel=~A value=~A ~A"
                       channel temp timestamp))
         (url (format nil "http://~A:~A/api/v2/write?org=~A&bucket=~A&precision=ms"
                      influxdb-host influxdb-port influxdb-org influxdb-bucket)))
    (format t "Influxing Data:~%~A~%" data)

    (unless dry
      (drakma:http-request url
                           :method :post
                           :content-type "text/plain; charset=utf-8"
                           :accept "application/json"
                           :additional-headers `(("Authorization" . ,(format nil "Token ~A" influxdb-token)))
                           :content data))))

;; Example usage
(push-data-to-influxdb 22 "salon" :dry t)
;; (push-data-to-influxdb 22 "salon")

(main)
