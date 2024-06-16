;; load `json.lisp' and `mqtt.lisp' first
(ql:quickload "local-time")

;; -------------- Framework code --------------
(defun init-timer (fn)
  (make-timer fn :thread t))

(defun reset-timer (timer interval)
  (unschedule-timer timer)
  (schedule-timer timer interval))

(defun stop-timer (timer)
  (unschedule-timer timer))

;; This will print to stdout after ~2s
(let ((timer
        (init-timer (lambda () (format t "Hello timer~%")))))
  (reset-timer timer 4)
  (sleep 2)
  (reset-timer timer .2))

(defun string->number (input)
  ;; FIXME Unsafe AF
  (read-from-string input))

(string->number "12.3")
 ; => 12.3, 4

(defun topic->object-name (topic)
  (let ((start (search "-" topic))
        (end (or (search "/get" topic)
                 (search "/set" topic))))
    (if (not start)
        topic
        (subseq topic (+ 1 start) end))))

(topic->object-name "z2m/therm-test-name-1/set")
 ; => "test-name-1"
(topic->object-name "z2m/therm-test-name-1/get")
 ; => "test-name-1"
(topic->object-name "z2m/switch-something-something-2")
 ; => "something-something-2"
(topic->object-name "othertopic")
 ; => "othertopic"

(defun set-state (broker topic state)
  (publish broker
           (format nil "~A/set/state" topic)
           (if state "ON" "OFF")))

(with-fn-shadow ('publish #'fake-publish)
  (set-state nil "z2m/test-actuator" t))
; Publishing:
;  [broker] NIL
;  [topic] z2m/test-actuator/set/state
;  [payload] ON
;  => NIL

(with-fn-shadow ('publish #'fake-publish)
  (set-state nil "z2m/test-actuator" nil))
; Publishing:
;  [broker] NIL
;  [topic] z2m/test-actuator/set/state
;  [payload] OFF
;  => NIL

(defun set-brightness (broker topic brightness)
  (declare (type number brightness))
  (publish broker
           (format nil "~A/set/brightness" topic)
           (format nil "~A" brightness)))

(with-fn-shadow ('publish #'fake-publish)
  (set-brightness nil "z2m/test-light" 100))
; Publishing:
;  [broker] NIL
;  [topic] z2m/test-light/set/brightness
;  [payload] 100
;  => NIL

(defun app-handle-test (topic payload)
  "Test handler. Returns a string"
  (format nil "Test handler called: ~A ~A" topic payload))

(defun app-handle-default (topic payload)
  "Fallback handler for MQTT messages"
  (format t "No handler found: [topic] ~A [message] ~A~%"
          topic payload))

(app-handle-topic "z2m/test-sensor-0012" (string->ascii "somepayload"))
 ; => "Test handler called: z2m/test-sensor-0012 somepayload"

(app-handle-topic "topic/with/no_dashes" (string->ascii "p4yload"))
; No handler found: [topic] topic/with/no_dashes [message] p4yload
;  => NIL

(defun extract-message-type (str)
  "Extracts the chars between \"z2m/\" and \"-\"."
  (let ((start (if (search "z2m/" str)
                   (length "z2m/")
                   0)))
    (subseq str start (search "-" str))))

(extract-message-type "z2m/msgtype-object")
 ; => "msgtype"

(extract-message-type "noncompliantmsgtype")
 ; => "noncompliantmsgtype"

(defun app-handle-topic (topic payload)
  "Find and call a message handler for a given topic"
  ;; This is kind of a hack :)
  ;;
  ;; We take the topic prefix, i.e. any characters before the first
  ;; dash, and construct a function name from it, using the following
  ;; scheme: "app-handle-{prefix}". If that function exists, we call
  ;; it, if not, we invoke a default handler.
  (let* ((message-type (extract-message-type topic))
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

;; -------------- Application Logic --------------
(defparameter *thermostats* (make-hash-table :test 'equalp))

(defun thermostat-value (name)
  (let ((temperature (gethash name *thermostats*)))

    (when (not temperature)
      (format t "[default] ")
      (setf temperature 20))

    (format t "Thermostat for ~A: ~A~%" name
            temperature)

    temperature))

(defun set-thermostat-value (name value)
  (declare (type number value)
           (type string name))
  (format t "New thermostat for ~A: ~A~%" name value)
  (setf (gethash name *thermostats*) value))

;; Read values from disk or load defaults.
;; TODO: implement read from disk
(set-thermostat-value "bureau" 19)
(set-thermostat-value "chambre" 21)
(set-thermostat-value "rachel" 21)
; New thermostat for rachel: 21
;  => 21 (5 bits, #x15, #o25, #b10101)

(thermostat-value "rachel")
; Thermostat for rachel: 21
;  => 21 (5 bits, #x15, #o25, #b10101)
(thermostat-value "noexist")
; [default] Thermostat for noexist: 20
;  => 20 (5 bits, #x14, #o24, #b10100)

(defun publish-thermostat (broker name)
  (publish broker
           (format nil "z2m/therm-~A" name)
           (format nil "~A" (thermostat-value name))))

(defun app-handle-therm (topic payload)
  (let ((name (topic->object-name topic)))
    (cond
      ((search "/set" topic)
       (progn
         (set-thermostat-value name (string->number payload))
         (publish-thermostat *broker* name)))
      ((search "/get" topic)
       (publish-thermostat *broker* name))
      (t (format t "Unexpected format [topic] ~A~%" topic)))))

(defun print-current-time (stream)
  (local-time:format-timestring
   stream (local-time:now)
   :format '(:year "/" :month "/" :day "-"
             :hour ":" :min ":" :sec)))

(print-current-time nil)
 ; => "2024/6/16-22:25:21"

(defun outside-working-hours? (time)
  ;; Future work:
  ;; use delta w/ outside temperature to set the start time
  (let ((start 7)
        (end 18)
        (current (local-time:timestamp-hour time)))
    (or (> current end)
        (< current start))))

(outside-working-hours?
 (local-time:parse-timestring "2024-06-13T19:09:06"))
 ; => T

(outside-working-hours?
 (local-time:parse-timestring "2024-06-12T03:09:06"))
 ; => T

(outside-working-hours?
 (local-time:parse-timestring "2024-06-13T09:09:06"))
 ; => NIL

(defun app-handle-temp (topic payload)
  "React to a temperature sensor value"
  (let* ((name (topic->object-name topic))
         (heater (format nil "z2m/prise-~A" name))
         (therm (thermostat-value name))
         (temp (jv payload :temperature))
         (delta .2))

    (unless temp
      (format t "Unknown format: [~A] ~A~%"
              topic payload)
      (return-from app-handle-temp nil))

    (format t "[~A] [~A]: Temperature: ~A~%"
            (print-current-time nil) name temp)

    ;; Office is special:
    ;; - it operates only during working hours
    ;; - it can be disabled entirely
    ;; - this special handling can also be disabled
    (when (search "bureau" name)
      (when (not *force-office*)
        (when (or (not *enable-office*)
                  (outside-working-hours? (local-time:now)))
          (set-state *broker* heater nil)
          (return-from app-handle-temp nil)))

    (cond
      ((> temp (+ therm delta))
        (progn
          (format t "[~A]: ~A > ~A -> heater [~A] OFF"
                  name temp therm heater)
          (set-state *broker* heater nil)))

      ((< temp (- therm delta))
        (progn
          (format t "[~A]: ~A < ~A -> heater [~A] OFF"
                  name temp therm heater)
          (set-state *broker* heater t)))))))

(defparameter *enable-office* t)

(defun app-handle-enable (topic payload)
  "Enable a space heater"
  ;; For now, there is only one heater that can be enabled/disabled.
  (declare (ignore topic))
  (setf *enable-office* (search "on" payload))
  (set-state *broker* "z2m/prise-bureau" *enable-office*))

(defparameter *force-office* t)

(defun app-handle-force (topic payload)
  "Force-enable a space heater"
  ;; For now, there is only one heater that can be force-enabled.
  (declare (ignore topic))
  (setf *force-office* (search "on" payload)))

(defun is-brightness-up? (action)
  (search "brightness_move_up" action))

(defun is-brightness-down? (action)
  (search "brightness_move_down" action))

(defun is-on-off? (action)
  (or (search "on" action)
      (search "off" action)))

(defun set-all-lights (broker state)
  (loop for name in '("cuisine"
                      "entree"
                      "couloir"
                      "manger")
        do (set-state broker
                      (format nil "z2m/light-~A" name)
                      state)))

(defun app-handle-switch (topic payload)
  (let* ((action (jv payload :action))
         (name (topic->object-name topic))
         (light (format nil "z2m/light-~A" name))
         (light-state (search "on" action)))

    (cond
      ((is-brightness-up? action)
       (set-brightness *broker* light 255))

      ((is-brightness-down? action)
       (set-brightness *broker* light 20))

      ((is-on-off? action)
       (if (search "cuisine" topic)
           ;; special case: "cuisine" controls all the lights
           (set-all-lights *broker* light-state)
           ;; other switches control their respective light
           (set-state *broker* light light-state))))))

(defun make-switch-payload (action-string)
  (json:encode-json-to-string
   (list
    (cons :battery 74)
    (cons :linkquality 120)
    (cons :action action-string))))

(make-switch-payload "brightness_move_up")
 ; => "{\"battery\":74,\"linkquality\":120,\"action\":\"brightness_move_up\"}"

;; Test invalid json payload
(with-fn-shadow ('publish #'fake-publish)
  (app-handle-switch "z2m/switch-chambre"
                     (json:encode-json-to-string
                      (list
                       (cons :battery 74)
                       (cons :linkquality 120)
                       (cons :thisisnotaction "somestring")))))
 ; => NIL

(with-fn-shadow ('publish #'fake-publish)
  (app-handle-topic "z2m/switch-chambre"
                     (string->ascii (make-switch-payload "brightness_move_down"))))

;; Test valid inputs
(with-fn-shadow ('publish #'fake-publish)
  (app-handle-switch "z2m/switch-chambre"
                     (make-switch-payload "brightness_move_down"))

  (app-handle-switch "z2m/switch-chambre"
                     (make-switch-payload "brightness_move_up"))

  (app-handle-switch "z2m/switch-bureau"
                     (make-switch-payload "off"))

  (app-handle-switch "z2m/switch-bureau"
                     (make-switch-payload "on")))
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-chambre/set/brightness
;  [payload] 20
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-chambre/set/brightness
;  [payload] 255
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-bureau/set/state
;  [payload] OFF
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-bureau/set/state
;  [payload] ON
;  => NIL

(with-fn-shadow ('publish #'fake-publish)
  ;; Test "light-cuisine" turns on all the lights
  (app-handle-switch "z2m/switch-cuisine"
                     (make-switch-payload "on")))
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-cuisine/set/state
;  [payload] ON
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-entree/set/state
;  [payload] ON
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-couloir/set/state
;  [payload] ON
; Publishing:
;  [broker] NIL
;  [topic] z2m/light-manger/set/state
;  [payload] ON
;  => NIL

(defparameter *door-timer*
  (init-timer
   (lambda () (set-state *broker* "z2m/light-entree" nil))))

(defparameter *door-timeout* (* 60 5))

(defun app-handle-door (topic payload)
  (declare (ignore topic))
  (let ((door-open (not (jv payload :contact)))
        (light (format nil "z2m/light-entree")))
    (when door-open
      ;; turn on the light
      (set-state *broker* light t)
      ;; turn off the light 5 minutes later
      (reset-timer *door-timer* *door-timeout*))))

(defun make-door-payload (contact)
  (json:encode-json-to-string
   (list
    (cons :contact contact)
    (cons :battery 74)
    (cons :device_temperature 120)
    (cons :linkquality 47)
    (cons :power_outage_count 184)
    (cons :voltage 2985))))

(make-door-payload t)
 ; => "{\"contact\":true,\"battery\":74,\"device_temperature\":120,\"linkquality\":47,\"power_outage_count\":184,\"voltage\":2985}"

;; To test this we have to:
;; - reduce the 5 minutes timeout to something lower
;; - keep shadowing PUBLISH until the timeout fires
(with-fn-shadow ('publish #'fake-publish)
  (let ((aint-nobody-got-time 3))
    (with-var-shadow ('*door-timeout* aint-nobody-got-time)
      (app-handle-topic "z2m/door-entree"
                        (string->ascii (make-door-payload nil)))
      (sleep (+ 1 aint-nobody-got-time)))))

(with-fn-shadow ('publish #'fake-publish)
  (app-handle-topic "z2m/door-entree"
                    (string->ascii (make-door-payload t))))

;; -------------- Entrypoint --------------

(mqtt-connect-to-broker "192.168.10.175" 1883 #'app-callback)

(subscribe *broker* "test/topic")
(publish *broker* "test/topic" "important data")
(progn (disconnect *broker*) (setf *broker* nil))





























;; ---------------------- SCRATCHPAD ----------------------
(mqtt-with-broker ("192.168.10.175" 1883 *broker*)
    (publish broker "z2m/light-manger/set/state" "ON")
    (sleep 1)
    (publish broker "z2m/light-manger/set/state" "OFF")
    )

(mqtt-with-broker ("192.168.10.175" 1883 broker)
  (set-brightness broker "z2m/light-chambre" 20))

(mqtt-with-broker ("192.168.10.175" 1883 broker)
    (publish broker "z2m/light-chambre/set/state" "OFF"))

(mqtt-with-broker ("192.168.10.175" 1883 broker)
    (publish broker "z2m/light-chambre/set/state" "ON"))
