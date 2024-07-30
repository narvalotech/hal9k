(ql:quickload :alexandria)

;; Web server framework
(ql:quickload :clack)
;; HTML with s-exps
(ql:quickload :spinneret)
;; CSS with s-exps
(ql:quickload :lass)
;; JS with s-exps
(ql:quickload :parenscript)

;; To decode/encode the MQTT payloads
(ql:quickload :cl-json)
;; To talk to MQTT (obv)
(ql:quickload :cl-mqtt)

;; What we need:
;; - light controls
;;   - bedroom (on/dim)
;;   - hallway (on)
;;   - entrance (on/dim)
;;   - kitchen counter (on)
;;   - kitchen table (on/dim)
;; - climate control
;;   - bedroom (temp)
;;   - rachel (temp)
;;   - office (on/force/temp)
;;
;; rooms:
;; - bedroom
;; - rachel's bedroom
;; - living room
;; - office
;; - rest of house

(defun decode-type/name/value (payload)
  (let ((decoded (json:decode-json-from-string payload)))
    (mapcar #'cdr decoded)))

(decode-type/name/value "{\"type\":\"button\",\"name\":\"chambre\",\"value\":\"toggle\"}")
 ; => ("button" "chambre" "toggle")

(defun publish (topic value)
  (mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
    (mqtt:publish broker topic value)))

(defun toggle-light (name)
  ;; TODO: return new state
  (publish (format nil "z2m/light-~A/set" name) "TOGGLE"))

(defun set-brightness (name brightness)
  (declare (type string brightness))
  (publish (format nil "z2m/light-~A/set/brightness" name)
           brightness))

(defun handle-light (payload)
  (destructuring-bind (type name value) (decode-type/name/value payload)
    (alexandria:switch (type :test #'equal)
      ("slider" (progn (format t "Handle slider: ~A val ~A~%" name value)
                       (set-brightness name value)))
      ("button" (progn (format t "Handle button: ~A val ~A~%" name value)
                       (toggle-light name)))
      (t (format t "Unknown type ~A~%" type)))))

(defun render-slider (name class)
  (spinneret:with-html-string
    (:li (:input :type :range
                 :class class
                 :name name
                 :min 0 :max 255 :step 1))))

(defun render-light-slider (name)
  (render-slider name "light-slider"))

(defun render-toggle (name class)
  (spinneret:with-html-string
    (:li (:button
          :class class
          :name name
          :value name name))))

(defun render-light-toggle (name)
  (render-toggle name "light-toggle"))

(defparameter jsmain
  (ps:ps
    (defun send-put-request (url type name value)
      (let ((data (ps:create :type type :name name :value value)))
        (ps:chain (fetch url
                         (ps:create :method "PUT"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body (ps:chain *json* (stringify data))))
                  (then (lambda (response) (ps:@ response json)))
                  (then (lambda (data) (ps:@ console (log data)))))))

    ;; note: use "input" for events on value change
    (defun setup-slider-event-listeners ()
      (let ((sliders (ps:chain document (get-elements-by-class-name "light-slider"))))
        (loop for slider across sliders
              do (ps:chain slider (add-event-listener
                                   "change"
                                   (lambda () (send-put-request "/light" "slider"
                                                                (ps:@ slider name)
                                                                (ps:@ slider value))))))))

    (defun setup-button-event-listeners ()
      (let ((buttons (ps:chain document (get-elements-by-class-name "light-toggle"))))
        (loop for button across buttons
              do (ps:chain button (add-event-listener
                                   "click"
                                   (lambda () (send-put-request "/light" "button"
                                                                (ps:@ button name)
                                                                ;; TODO: use 'value' attr
                                                                "toggle")))))))

    (setup-slider-event-listeners)
    (setup-button-event-listeners)))

(defun controls ()
  (spinneret:with-html-string
    (:doctype)
    (:head)
    (:body
     (:p "Lights")
     (:ul
      (:raw
       (render-light-slider "chambre")
       (render-light-toggle "chambre")
       (render-light-toggle "couloir")))
     (:script (:raw jsmain)))))

(defun response (env)
  (format t "env: ~A~%" env)

  (when (equal (getf env :request-method) :PUT)
    (let* ((stream (getf env :raw-body))
           (payload (read-line stream)))

      (format t "=> payload: ~A~%" payload)

      (when (equal (getf env :request-uri) "/light")
        (handle-light payload))))

  ;; TODO: read current values from MQTT

  ;; Response:
  ;; [status code] [headers (plist)] [body (strings / vector / pathname)]
  ;; TODO: err status code when MQTT send fails
  (list 200 '(:content-type "text/html") (list (controls))))

(defvar *handler*
  (clack:clackup
   'response))

(clack:stop *handler*)
