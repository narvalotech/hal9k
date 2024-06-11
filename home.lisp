;; load `json.lisp' and `mqtt.lisp' first

;; -------------- Application Logic --------------
(defun app-handle-temp (topic payload)
  "React to a temperature sensor value"
  (break))

(defparameter *enable-office* t)

(defun app-handle-enable (topic payload)
  "Enable a space heater"
  ;; For now, there is only one heater that can be enabled/disabled.
  (declare (ignore topic))
  (setf *enable-office* (search "on" payload))
  (publish *broker* "z2m/prise-bureau" (make-onoff *enable-office*)))

(defparameter *force-office* t)

(defun app-handle-force (topic payload)
  "Force-enable a space heater"
  ;; For now, there is only one heater that can be force-enabled.
  (declare (ignore topic))
  (setf *force-office* (search "on" payload)))

;; -------------- Framework code --------------
(defun make-onoff ())
(defun app-handle-test (topic payload)
  "Test handler. Returns a string"
  (format nil "Test handler called: ~A ~A" topic payload))

(defun app-handle-default (topic payload)
  "Fallback handler for MQTT messages"
  (format t "No handler found: [topic] ~A [message] ~A~%"
          topic payload))

(app-handle-topic "switch-bureau-12" "somepayload")
(app-handle-topic "temp-bureau-12" "somepayload")

(app-handle-topic "test-sensor-0012" "somepayload")
 ; => "Test handler called: test-sensor-0012 somepayload"

(app-handle-topic "topic/with/no_dashes" "p4yload")
; No handler found: [topic] topic/with/no_dashes [message] p4yload
;  => NIL

(defun chars-before-dash (str)
  "Returns the characters before the first \"-\""
  (subseq str 0 (search "-" str)))

(defun app-handle-topic (topic payload)
  "Find and call a message handler for a given topic"
  ;; This is kind of a hack :)
  ;;
  ;; We take the topic prefix, i.e. any characters before the first
  ;; dash, and construct a function name from it, using the following
  ;; scheme: "app-handle-{prefix}". If that function exists, we call
  ;; it, if not, we invoke a default handler.
  (let* ((message-type (chars-before-dash topic))
         (handler-name (format nil "app-handle-~A" message-type))
         (handler-fn (read-from-string handler-name)))

    (if (not (fboundp handler-fn))
             (setf handler-fn #'app-handle-default))

    ;; We assume that our home automation stuff only ever sends utf-8
    ;; strings as payload.
    (setf payload (ascii->string payload))

    (funcall handler-fn topic payload)))

(defun app-process-packet (packet)
  ;; For now, we just parse it to stdout
  (if (> (length packet) 0)
      (let ((parsed (mqtt-parse-packet packet)))
        (if (> (length packet) 0)
            (case (first parsed)
              (:pingrsp nil)
              (:publish (app-handle-topic
                         (getf (cdr parsed) :topic)
                         (getf (cdr parsed) :payload)))
              (t (format t "Got packet: ~A~%" parsed)))))))

(app-process-packet
 (mqtt-make-packet :publish
                   :topic "test-topic/something"
                   :payload (string->ascii "my-payload 1234 56")))
 ; => "Test handler called: test-topic/something my-payload 1234 56"

(defun app-callback (broker data)
  (setf *broker* broker)
  (app-process-packet data))

;; TODO: create other thread for timeouts
(mqtt-connect-to-broker "localhost" 1883 #'app-callback)



(subscribe *broker* "test/topic")
(publish *broker* "test/topic" "important data")
(disconnect *broker*)





























;; ---------------------- SCRATCHPAD ----------------------
(mqtt-with-broker ("192.168.10.175" 1883 *broker*)
    (publish broker "z2m/light-manger/set/state" "ON")
    (sleep 1)
    (publish broker "z2m/light-manger/set/state" "OFF")
    )

(mqtt-with-broker ("192.168.10.175" 1883 broker)
    (publish broker "z2m/light-chambre/set/state" "OFF"))

(mqtt-with-broker ("192.168.10.175" 1883 broker)
    (publish broker "z2m/light-chambre/set/state" "ON"))
