(defpackage #:home/tests
  (:use :cl :fiveam)
  (:import-from #:home
                #:set-state
                #:set-brightness
                #:app-handle-switch
                #:app-handle-topic
                #:make-switch-payload
                :*door-timeout*
                )
  (:local-nicknames
   (#:mqtt #:cl-mqtt)
   (#:json #:cl-json))
  (:export #:run-all))

(in-package :home/tests)

;; (setf fiveam:*run-test-when-defined* t)

(defun run-all ()
  (run!))

(defmacro with-fn-shadow ((orig new) &body body)
  `(let ((orig-backup))                 ; TODO: use gensym
     (if (fboundp ,orig)
         (progn
           (setf orig-backup (symbol-function ,orig))
           (setf (symbol-function ,orig) ,new)
           (unwind-protect (progn ,@body)
             (setf (symbol-function ,orig) orig-backup)))
         (error "Function ~A is not defined" ,orig))))

(defmacro with-var-shadow ((orig new) &body body)
  `(let ((orig-backup))                 ; TODO: use gensym
     (if (boundp ,orig)
         (progn
           (setf orig-backup (symbol-value ,orig))
           (setf (symbol-value ,orig) ,new)
           (unwind-protect (progn ,@body)
             (setf (symbol-value ,orig) orig-backup)))
         (error "Variable ~A is not defined" ,orig))))

(defun fake-publish (broker topic payload)
  (list :broker broker :topic topic :payload payload))

(test set-state
  (with-fn-shadow ('mqtt:publish #'fake-publish)
    (is (equal '(:broker nil :topic "z2m/test-actuator/set/state" :payload "ON")
               (set-state nil "z2m/test-actuator" t)))

    (is (equal '(:broker nil :topic "z2m/test-actuator/set/state" :payload "OFF")
               (set-state nil "z2m/test-actuator" nil)))
    ))

(test set-brightness
  (with-fn-shadow ('mqtt:publish #'fake-publish)
    (is (equal '(:BROKER NIL :TOPIC "z2m/test-light/set/brightness" :PAYLOAD "100")
               (set-brightness nil "z2m/test-light" 100)))))

(test invalid-inputs
  (with-fn-shadow ('mqtt:publish #'fake-publish)
    (is-false
     (app-handle-switch "z2m/switch-chambre"
                        (json:encode-json-to-string
                         (list
                          (cons :battery 74)
                          (cons :linkquality 120)
                          (cons :thisisnotaction "somestring")))))))

(test switch-handling
  (with-fn-shadow ('mqtt:publish #'fake-publish)
    (is (equal '(:BROKER NIL :TOPIC "z2m/light-chambre/set/brightness" :PAYLOAD "20")
               (app-handle-topic "z2m/switch-chambre"
                                 (mqtt:string->ascii (make-switch-payload "brightness_move_down")))))
    (is (equal '(:BROKER NIL :TOPIC "z2m/light-chambre/set/brightness" :PAYLOAD "20")
               (app-handle-switch "z2m/switch-chambre"
                                  (make-switch-payload "brightness_move_down"))))

    (is (equal '(:BROKER NIL :TOPIC "z2m/light-chambre/set/brightness" :PAYLOAD "255")
               (app-handle-switch "z2m/switch-chambre"
                                  (make-switch-payload "brightness_move_up"))))

    (is (equal '(:BROKER NIL :TOPIC "z2m/light-bureau/set/state" :PAYLOAD "OFF")
               (app-handle-switch "z2m/switch-bureau"
                                  (make-switch-payload "off"))))

    (is (equal '(:BROKER NIL :TOPIC "z2m/light-bureau/set/state" :PAYLOAD "ON")
               (app-handle-switch "z2m/switch-bureau"
                                  (make-switch-payload "on"))))


    ))

;; TODO: verify this. This needs a stream of sorts that ends up in a list, as
;; there are multiple writes.
(with-fn-shadow ('mqtt:publish #'fake-publish)
  ;; Test "light-cuisine" turns on all the lights
  (app-handle-switch "z2m/switch-cuisine"
                     (make-switch-payload "on")))

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
(with-fn-shadow ('mqtt:publish #'fake-publish)
  (let ((aint-nobody-got-time .5))
    (with-var-shadow ('*door-timeout* aint-nobody-got-time)
      (app-handle-topic "z2m/door-entree"
                        (mqtt:string->ascii (make-door-payload nil)))
      (sleep (+ .2 aint-nobody-got-time)))))

(with-fn-shadow ('mqtt:publish #'fake-publish)
  (app-handle-topic "z2m/door-entree"
                    (mqtt:string->ascii (make-door-payload t))))
