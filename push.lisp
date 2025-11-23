(push "/home/john/hal9k/" ql:*local-project-directories*)

(ql:quickload "drakma")
(ql:quickload "local-time")
(ql:quickload "cl-json")
(ql:quickload "cl-mqtt")

;; get'em debug frames
;; (declaim (optimize (debug 3)))

;; (defparameter *influx-host* "192.168.10.175")
(defparameter *influx-host* "127.0.0.1")

(defun push-data-to-influxdb (value-name value channel &key dry)
  (let* ((influxdb-host *influx-host*)
         (influxdb-port 8086)
         (influxdb-org "org")
         (influxdb-bucket "series")
         (influxdb-token "homesweethome")
         (timestamp (format nil "~D" (* 1000 (local-time:timestamp-to-unix (local-time:now)))))
         (data (format nil "~A,channel=~A value=~A ~A"
                       value-name channel value timestamp))
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
(push-data-to-influxdb "temperature" 22 "salon" :dry t)
;; (push-data-to-influxdb 22 "salon")

(defun notify-phone (text)
  (drakma:http-request
   *notify-url*
   :method :post
   :basic-authorization (list "" *token*)
   :content text))

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
        (temp (jv payload :temperature))
        (humd (jv payload :humidity)))

    (unless temp
      (format t "Unknown format: [~A] ~A~%"
              topic payload)
      (return-from log-temperature nil))

    ;; (push-data-to-influxdb temp name :dry t)
    (push-data-to-influxdb "temperature" temp name)
    (push-data-to-influxdb "humidity" humd name)))

(defun notify-door (topic payload)
  (let ((name (topic->object-name topic))
        (closed (jv payload :contact)))

    (notify-phone
     (format nil "~A: ~A"
             name
             (if closed "closed" "open")))))

(defun app-handle-topic (topic payload)
  "Find and call a message handler for a given topic"
  (when (or (search "z2m/temp" topic)
            (search "z2m/door-cuisine" topic))

    ;; Assume utf-8 strings
    (setf payload (mqtt:ascii->string payload))

    (format t "handling: [~A] ~A~%" topic payload)
    (when (search "z2m/temp" topic)
      (log-temperature topic payload))

    (when (search "z2m/door-cuisine" topic)
      (notify-door topic payload))))

(defun app-process-packet (parsed)
  (if parsed
      (case (first parsed)
        (:pingrsp nil)
        (:publish (app-handle-topic
                   (getf (cdr parsed) :topic)
                   (getf (cdr parsed) :payload)))
        (t (format t "Got packet[~A]: ~X~%" (length parsed) parsed)))))

(defparameter *broker* nil)

(defun app-callback (broker data)
  (setf *broker* broker)
  (when (> (length data) 0)
    (mapcar #'app-process-packet
            (mqtt:parse-packets (coerce data 'list)))))

(defun main ()
  (handler-case (mqtt:connect-to-broker "127.0.0.1" 1883 #'app-callback)
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (format *error-output* "Caught interrupt, aborting~%")
           (uiop:quit)))
    (error (c) (format t "Unknown error occured:~&~a~&" c))))

(main)
