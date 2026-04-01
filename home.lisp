(require 'asdf)

;; open SLYNK server for remote debugging
;; slynk is installed in:
;; (ql:where-is-system :slynk)
;; (ql:quickload "slynk")
(require :slynk)

(require :cl-json)
(require :local-time)
(require :cl-mqtt)
(require :trivial-timer)
(require :bordeaux-threads)

(defpackage :home
  (:use :common-lisp)

  (:import-from :cl-mqtt
                #:string->ascii
                #:ascii->string)

  (:local-nicknames
   (#:time #:local-time)
   (#:json #:cl-json)))

(in-package :home)

;; get'em debug frames
(declaim (optimize (debug 3)))

(defun jv (json-string key)
  "Returns the value pointed to by `KEY' in `JSON-STRING'. Use like `GETF'."
  (let ((decoded (json:decode-json-from-string json-string)))
    (loop for el in decoded do
          (if (equal (car el) key) (return (cdr el))))))

;; -------------- Framework code --------------
(defparameter *timer* nil)

(defun init-timer (fn)
  (trivial-timer:initialize-timer)
  (setf *timer*
        (list nil fn)))

(defun reset-timer (timer interval)
  (declare (ignore timer))
  (destructuring-bind (id fn) *timer*
    (when id
      (trivial-timer:cancel-timer-call id))
    (setf *timer*
          (list
           (trivial-timer:register-timer-call (floor (* 1000 interval)) fn)
           fn))))

(defun stop-timer (timer)
  (declare (ignore timer))
  (destructuring-bind (id fn) *timer*
    (declare (ignore fn))
    (when id
      (trivial-timer:cancel-timer-call id))))

;; This will print to stdout after ~.2s
;; (let ((timer
;;         (init-timer (lambda () (format t "Hello timer~%")))))
;;   (reset-timer timer 4)
;;   (sleep .1)
;;   (reset-timer timer .2))

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
  (mqtt:publish broker
           (format nil "~A/set/state" topic)
           (if state "ON" "OFF")))

(defun set-brightness (broker topic brightness)
  (declare (type number brightness))
  (mqtt:publish broker
           (format nil "~A/set/brightness" topic)
           (format nil "~A" brightness)))

(defun app-handle-test (topic payload)
  "Test handler. Returns a string"
  (format nil "Test handler called: ~A ~A" topic payload))

(defun app-handle-default (topic payload)
  "Fallback handler for MQTT messages"
  (declare (ignore topic payload))
  nil)
  ;; (push
  ;;  (list :topic topic :payload payload)
  ;;  *unknowns*)
  ;; (format *error-output* "No handler: [topic] ~A [message] ~A~%"
  ;;         topic payload))

(defun null-route (topic payload)
  ;; (format t "discarding: ~A~%" payload)
  (declare (ignore topic payload))
  nil)

(defun app-handle-prise (topic payload)
  (null-route topic payload))

(defun app-handle-light (topic payload)
  (null-route topic payload))

(defun app-handle-bridge/logging (topic payload)
  ;; null-route verbose logging messages
  (null-route topic payload))

(defun app-handle-bridge/log (topic payload)
  ;; null-route verbose logging messages
  (null-route topic payload))

(defun extract-message-type (str)
  "Extracts the chars between \"z2m/\" and \"-\"."
  (let ((start (if (search "z2m/" str)
                   (length "z2m/")
                   0)))
    (subseq str start (search "-" str))))

(extract-message-type "z2m/bridge/logging")
 ; => "bridge/logging"

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

(app-handle-topic "z2m/test-sensor-0012" (string->ascii "somepayload"))
 ; => "Test handler called: z2m/test-sensor-0012 somepayload"

(app-handle-topic "topic/with/no_dashes" (string->ascii "p4yload"))
; No handler found: [topic] topic/with/no_dashes [message] p4yload
;  => NIL

(defun app-process-packet (parsed)
  (if parsed
      (case (first parsed)
        (:pingrsp nil)
        (:publish (app-handle-topic
                   (getf (cdr parsed) :topic)
                   (getf (cdr parsed) :payload)))
        (t (format t "Got packet[~A]: ~X~%" (length parsed) parsed)))))

(app-process-packet
 (mqtt:parse-packet
  (mqtt:make-packet :publish
                    :topic "test-topic/something"
                    :payload (string->ascii "my-payload 1234 56"))))
 ; => "Test handler called: test-topic/something my-payload 1234 56"

(defparameter *broker* nil)

(defun app-callback (broker data)
  (setf *broker* broker)
  (when (> (length data) 0)
    (mapcar #'app-process-packet
            (mqtt:parse-packets (coerce data 'list)))))

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
(set-thermostat-value "salon" 20)
; New thermostat for rachel: 21
;  => 21 (5 bits, #x15, #o25, #b10101)

(thermostat-value "rachel")
; Thermostat for rachel: 21
;  => 21 (5 bits, #x15, #o25, #b10101)
(thermostat-value "noexist")
; [default] Thermostat for noexist: 20
;  => 20 (5 bits, #x14, #o24, #b10100)

(defun publish-thermostat (broker name)
  (mqtt:publish broker
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
       (publish-thermostat *broker* name)))))

(defun print-current-time (stream)
  (time:format-timestring
   stream (time:now)
   :format '(:year "/" :month "/" :day "-"
             :hour ":" :min ":" :sec)))

(print-current-time nil)
 ; => "2024/6/16-22:25:21"

(defun outside-working-hours? (time)
  ;; Future work:
  ;; use delta w/ outside temperature to set the start time
  (let ((start 7)
        (end 18)
        (current (time:timestamp-hour time)))
    (or (> current end)
        (< current start))))

(outside-working-hours?
 (time:parse-timestring "2024-06-13T19:09:06"))
 ; => T

(outside-working-hours?
 (time:parse-timestring "2024-06-12T03:09:06"))
 ; => T

(outside-working-hours?
 (time:parse-timestring "2024-06-13T09:09:06"))
 ; => NIL

(defparameter *enable-office* t)
(defparameter *force-office* nil)

(defparameter *temperatures* (make-hash-table :test 'equalp))

(defun cache-temp (name value)
  (setf (gethash name *temperatures*) value))

(defun get-cached-temp (name)
  (gethash name *temperatures*))

(defun publish-temp (broker name value)
  (mqtt:publish broker
           (format nil "z2m/cached/temp-~A" name)
           (format nil "~A" value)))

(defun app-handle-cached/temp (topic payload)
  (declare (ignore payload))
  (let* ((name (topic->object-name topic))
         (temp (gethash name *temperatures*)))

    (cond
      ((search "/get" topic)
       (publish-temp *broker* name (if temp temp 0))))))

(defun publish-enable (broker name)
  (let ((enable (if (search "bureau" name)
                    *enable-office*
                    t)))
    (mqtt:publish broker
                  (format nil "z2m/cached/enable-~A" name)
                  (format nil "~A" (if enable "ON" "OFF")))))

(defun app-handle-cached/enable (topic payload)
  (declare (ignore payload))
  (let ((name (topic->object-name topic)))
    (cond
      ((search "/get" topic)
       (publish-enable *broker* name)))))

(defun app-handle-temp (topic payload)
  "React to a temperature sensor value"
  (let* ((name (topic->object-name topic))
         (heater (format nil "z2m/prise-~A" name))
         (humidifier (format nil "z2m/humidifier-~A" name))
         (therm (thermostat-value name))
         (temp (jv payload :temperature))
         (humd (jv payload :humidity))
         (delta .2))

    (unless temp
      (format t "Unknown format: [~A] ~A~%"
              topic payload)
      (return-from app-handle-temp nil))

    (format t "[~A] [~A]: Temperature: ~A Humidity: ~A~%"
            (print-current-time nil) name temp humd)

    (cache-temp name temp)

    (cond
      ((> temp (+ therm delta))
       (progn
         (format t "[~A]: ~A > ~A -> heater [~A] OFF~%"
                 name temp therm heater)
         (set-state *broker* heater nil)))

      ((< temp (- therm delta))
       (progn
         (format t "[~A]: ~A < ~A -> heater [~A] ON~%"
                 name temp therm heater)
         (set-state *broker* heater t))))

    (let ((humidity-min 42)
          (humidity-max 45))
      (cond
        ((> humd humidity-max)
         (progn
           (format t "[~A]: ~A > ~A -> humidifier [~A] OFF~%"
                   name humd humidity-max humidifier)
           (when (search "chambre" humidifier)
             (set-state *broker* "z2m/humidifier-rachel" nil))
           (set-state *broker* humidifier nil)))

        ((< humd humidity-min)
         (progn
           (format t "[~A]: ~A < ~A -> humidifier [~A] ON~%"
                   name humd humidity-min humidifier)
           (when (search "chambre" humidifier)
             (set-state *broker* "z2m/humidifier-rachel" t))
           (set-state *broker* humidifier t)))))))

(defun app-handle-enable (topic payload)
  "Enable a space heater"
  ;; For now, there is only one heater that can be enabled/disabled.
  (declare (ignore topic))
  (format t "Heater bureau: ~A~%" payload)
  (setf *enable-office* (equalp "on" payload))
  (set-state *broker* "z2m/prise-bureau" *enable-office*))

(defun app-handle-force (topic payload)
  "Force-enable a space heater"
  ;; For now, there is only one heater that can be force-enabled.
  (declare (ignore topic))
  (setf *force-office* (equalp "on" payload)))

(defun is-brightness-up? (action)
  (search "brightness_move_up" action))

(defun is-brightness-down? (action)
  (search "brightness_move_down" action))

(defun is-on-off? (action)
  (or (equalp "on" action)
      (equalp "off" action)))

;; TODO: put bedroom at end of list
(defparameter *stationary-lights* '())
(defparameter *all-lights* *stationary-lights*)

(defun update-active-lights (lights)
  (setf *all-lights*
        (remove-duplicates (append *stationary-lights* lights)
                           :from-end t
                           :test #'equalp)))

(defun extract-friendly-name (device)
  (cdr (assoc :friendly--name device)))

(defun extract-light-name (str)
  (subseq str (+ 1 (search "-" str))))

(defun store-lights (devices-json)
  (let ((lights (remove-if-not
                 (lambda (x) (search "light-" (extract-friendly-name x)))
                 (json:decode-json-from-string devices-json))))
    (update-active-lights
     (mapcar (lambda (x) (extract-light-name
                          (extract-friendly-name x)))
             lights))))

(defun app-handle-bridge/devices (topic payload)
  (store-lights payload))

(defun app-handle-cached/lights/get (topic payload)
  (declare (ignore topic payload))
  (mqtt:publish *broker*
                (format nil "z2m/cached/lights")
                (format nil "~A" *all-lights*)))

(defun app-handle-cached/lights (topic payload)
  (null-route topic payload))

(defun set-all-lights (broker state)
  (loop for name in *all-lights*
        do (set-state broker
                      (format nil "z2m/light-~A" name)
                      state)))

(defun app-handle-switch (topic payload)
  (let* ((action (jv payload :action))
         (name (topic->object-name topic))
         (light (format nil "z2m/light-~A" name))
         (light-state (equalp "on" action)))

    (cond
      ;; ((search "kitchen" topic)
      ;;  ;; special case: "cuisine" controls all the lights
      ;;  ;; long-presses turn on/off the big halogen light
      ;;  (cond
      ;;    ((is-brightness-up? action)
      ;;     (set-state *broker* "z2m/light-chonk" t))
      ;;    ((is-brightness-down? action)
      ;;     (set-state *broker* "z2m/light-chonk" nil))
      ;;    ((is-on-off? action)
      ;;     (set-all-lights *broker* light-state))))
      ((search "hallway" topic)
       (cond
         ((is-brightness-up? action)
          (set-state *broker* "z2m/light-door" t))
         ((is-brightness-down? action)
          (set-state *broker* "z2m/light-door" nil))
         ((is-on-off? action)
          (set-state *broker* light light-state))))
      ;; Other switches control their respective lights
      (t
       (cond
         ((is-brightness-up? action)
          (set-brightness *broker* light 255))
         ((is-brightness-down? action)
          (set-brightness *broker* light 20))
         ((is-on-off? action)
          (set-state *broker* light light-state)))))))

(defun make-switch-payload (action-string)
  (json:encode-json-to-string
   (list
    (cons :battery 74)
    (cons :linkquality 120)
    (cons :action action-string))))

(make-switch-payload "brightness_move_up")
 ; => "{\"battery\":74,\"linkquality\":120,\"action\":\"brightness_move_up\"}"

(defparameter *door-timer*
  (init-timer
   (lambda () (set-state *broker* "z2m/light-door" nil))))

(defparameter *door-timeout* (* 60 5))

(defun app-handle-door (topic payload)
  (declare (ignore topic))
  (let ((door-open (not (jv payload :contact)))
        (light (format nil "z2m/light-door")))
    (when door-open
      ;; turn on the light
      (set-state *broker* light t)
      ;; turn off the light 5 minutes later
      (reset-timer *door-timer* *door-timeout*))))

(format t "Done eval-ing~%")

;; ccl --eval '(ql:quickload :slynk)' --eval '(slynk:create-server :port 42069 :dont-close t)'

(defun slynk-listener-thread-p (thread)
  "Check if the given thread is a Slynk listener thread."
  (let ((thread-name (bt:thread-name thread)))
    (format t "thread: ~A~%" thread-name)
    (and thread-name
         (search "slynk" thread-name :test #'equalp))))

(defun slynk-server-running-p ()
  "Check if there is any active Slynk listener thread."
  t)
  ;; (some #'slynk-listener-thread-p (bt:all-threads)))

(defparameter *home-server* "127.0.0.1")
;; (defparameter *home-server* "192.168.10.150")
;; (defparameter *home-server* "192.168.10.175")

(defun main ()
  (unless (slynk-server-running-p)
    (format t "Starting SLYNK server~%")
    (slynk:create-server :port 42169 :dont-close t))

  (handler-case (mqtt:connect-to-broker *home-server* 1883 #'app-callback)
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
      () (progn
           (format *error-output* "Caught interrupt, aborting~%")
           (uiop:quit)))
    (error (c) (progn (format t "Unknown error occured:~&~a~&" c)
                      (uiop:quit)))))

#|
;; Bootstrap quicklisp:

Deploy with:

cd ~/repos/hal9k && rsync -av --exclude '.*' ../hal9k nas:~/

curl -O https://beta.quicklisp.org/quicklisp.lisp && \
sbcl --load quicklisp.lisp \
     --eval '(quicklisp-quickstart:install)' \
     --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
     --quit

How to install deps, load and run

sbcl --eval '(push "/home/jon/hal9k/" ql:*local-project-directories*)' \
     --eval '(ql:quickload "home")' \
     --eval "(in-package :home)" \
     --eval "(main)"

ccl -b --eval '(push "/home/jon/hal9k/" ql:*local-project-directories*)' \
     --eval '(ql:quickload "home")' \
     --eval "(in-package :home)" \
     --eval "(main)"

ccl -b --eval '(push "/home/jon/hal9k/" ql:*local-project-directories*)' \
     --load hal9k/web.lisp

|#
