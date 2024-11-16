(require :asdf)

(push "/home/john/hal9k/" ql:*local-project-directories*)

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

(defun decode-type/name/value (payload)
  (let ((decoded (json:decode-json-from-string payload)))
    (mapcar #'cdr decoded)))

(decode-type/name/value "{\"type\":\"button\",\"name\":\"chambre\",\"value\":\"toggle\"}")
 ; => ("button" "chambre" "toggle")

(defun gen-random-id ()
  (format nil "client-~A" (gensym)))

(defparameter *client-id* (gen-random-id))

(defun publish (topic value)
  (mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str *client-id*)
    (format t "############# PUBLISH: [~A] ~A~%" topic value)
    (mqtt:publish broker topic value)))

;; (defun publish (topic value)
;;   (format t "############# PUBLISH: [~A] ~A~%" topic value))

(defun toggle-light (name)
  ;; TODO: return new state
  (publish (format nil "z2m/light-~A/set" name) "TOGGLE"))

(defun set-light (name state)
  (publish (format nil "z2m/light-~A/set" name)
           (if (equal state "on") "ON" "OFF")))

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
                       (set-light name value)))
      (t (format t "Unknown type ~A~%" type)))))

(defun set-thermostat (name value)
  (publish (format nil "z2m/therm-~A/set" name)
           value))

(defun set-heat (name state)
  "Enable a space heater"
  (publish (format nil "z2m/enable-~A/set" name)
           (if (equal state "on") "ON" "OFF")))

(defun handle-heat (payload)
  (destructuring-bind (type name value) (decode-type/name/value payload)
    (alexandria:switch (type :test #'equal)
      ("button" (progn (format t "Handle button: ~A val ~A~%" name value)
                       (set-heat name value)))
      ("number" (progn (format t "Handle number: ~A val ~A~%" name value)
                       (set-thermostat name value)))
      (t (format t "Unknown type ~A~%" type)))))

(defparameter *svg-light-on*
  "<svg class=\"on\" xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"lucide lucide-lightbulb\">
                            <path d=\"M15 14c.2-1 .7-1.7 1.5-2.5 1-.9 1.5-2.2 1.5-3.5A6 6 0 0 0 6 8c0 1 .2 2.2 1.5 3.5.7.7 1.3 1.5 1.5 2.5\"/>
                            <path d=\"M9 18h6\"/>
                            <path d=\"M10 22h4\"/>
                        </svg>")

(defparameter *svg-light-off*
  "<svg class=\"off\" xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"lucide lucide-lightbulb-off\">
                            <path d=\"M16.8 11.2c.8-.9 1.2-2 1.2-3.2a6 6 0 0 0-9.3-5\"/>
                            <path d=\"m2 2 20 20\"/>
                            <path d=\"M6.3 6.3a4.67 4.67 0 0 0 1.2 5.2c.7.7 1.3 1.5 1.5 2.5\"/>
                            <path d=\"M9 18h6\"/>
                            <path d=\"M10 22h4\"/>
                        </svg>")

(defparameter *svg-heat-on*
  "<svg class=\"on\" xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"lucide lucide-flame\">
                            <path d=\"M8.5 14.5A2.5 2.5 0 0 0 11 12c0-1.38-.5-2-1-3-1.072-2.143-.224-4.054 2-6 .5 2.5 2 4.9 4 6.5 2 1.6 3 3.5 3 5.5a7 7 0 1 1-14 0c0-1.153.433-2.294 1-3a2.5 2.5 0 0 0 2.5 2.5z\"/>
                        </svg>")

(defparameter *svg-heat-off*
  "<svg class=\"off\" xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"lucide lucide-snowflake\">
                            <line x1=\"2\" x2=\"22\" y1=\"12\" y2=\"12\"/>
                            <line x1=\"12\" x2=\"12\" y1=\"2\" y2=\"22\"/>
                            <path d=\"m20 16-4-4 4-4\"/>
                            <path d=\"m4 8 4 4-4 4\"/>
                            <path d=\"m16 4-4 4-4-4\"/>
                            <path d=\"m8 20 4-4 4 4\"/>
                        </svg>")

(apply #'concatenate 'string '("hello" "world"))

(defun render-control-group (name &rest rendered-controls)
  (spinneret:with-html-string
    (:div :class "control-group"
          (:span :class "label" name)
          (:raw
           (apply #'concatenate 'string rendered-controls)))))

(defun render-light (name)
  (spinneret:with-html-string
    (:div :class "control light" :data-name name
          (:span :class "label light" name)
          (:button :class "switch light" :data-state "on"
                   :name "light"
                   (:raw *svg-light-on*)
                   (:raw *svg-light-off*))
          (:input :type :range
                  :class "slider light"
                  :min 0
                  :max 255
                  :step 1
                  :value 42))))

;; (format t "~A" (render-light "hello"))

(defun filter-publish (packet)
  (when packet
    (case (first packet)
      (:publish t))))

(defun get-thermostat-value (name)
  (read-from-string
   (mqtt:ascii->string
    (mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str (gen-random-id))
      (mqtt:publish-with-response broker
                                  (format nil "z2m/therm-~A/get" name) "0"
                                  (format nil "z2m/therm-~A" name)
                                  #'filter-publish)))))

(defun get-temp-value (name)
  (read-from-string
   (mqtt:ascii->string
    (mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str (gen-random-id))
      (mqtt:publish-with-response broker
                                  (format nil "z2m/cached/temp-~A/get" name) "0"
                                  (format nil "z2m/cached/temp-~A" name)
                                  #'filter-publish)))))

(defun get-enable-value (name)
  (string-downcase
   (mqtt:ascii->string
    (mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str (gen-random-id))
      (mqtt:publish-with-response broker
                                  (format nil "z2m/cached/enable-~A/get" name) "0"
                                  (format nil "z2m/cached/enable-~A" name)
                                  #'filter-publish)))))

;; maybe a slider + value display would be better?
(defun render-heater (name &optional enable-button)
  (spinneret:with-html-string
    (:div :class "control heat" :data-name name
          (:span :class "label heat" name)
          (:span :class "current-temperature"
                 (format nil "~2,1F" (get-temp-value name)))
          (when enable-button
            (:button :class "switch heat" :data-state (get-enable-value name)
                     :name "heat"
                     (:raw *svg-heat-on*)
                     (:raw *svg-heat-off*)))
          (:input :type :number
                  :class "num-input"
                  :inputmode :numeric
                  :enterkeyhint "done"
                  :value (get-thermostat-value name)
                  :min 10
                  :max 35))))

;; (format t "~A" (render-heater "hello"))

;; TODO: fix naming, it's all over the place
(defparameter main.js
  (ps:ps
    (defun send-put-request (url type name value)
      (let ((data (ps:create :type type :name name :value value)))
        (ps:chain (fetch url
                         (ps:create :method "PUT"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body (ps:chain *json* (stringify data))))
                  (then (lambda (response) (ps:@ response json)))
                  (then (lambda (data) (ps:@ console (log data)))))))

    (defun set-display-style (img-on img-off new-state)
      (let ((state (equal new-state "on")))
        (setf (ps:@ img-on style display) (if state "block" "none"))
        (setf (ps:@ img-off style display) (if state "none" "block"))))

    (defun toggle-state (data)
      (if (equal (ps:@ data state) "on")
          (setf (ps:@ data state) "off")
          (setf (ps:@ data state) "on")))

    (defun get-endpoint (type)
      (cond
        ((equal type "light") "/light")
        ((equal type "heat") "/heat")
        (t (error "not supported"))))

    (defun button-event-listener (img-on img-off type name data)
      (toggle-state data)
      (set-display-style img-on img-off (ps:@ data state))
      (send-put-request (get-endpoint type) "button" name (ps:@ data state)))

    (defun setup-event-listener (element-class event setup-fn)
      (let ((elements (ps:chain document (get-elements-by-class-name element-class))))
        (loop for element across elements
              do (ps:chain element (add-event-listener event (lambda () (setup-fn element)))))))

    (defun setup-change-event-listener (element-class url input-type)
      ;; note: use "input" for events on value change
      (setup-event-listener element-class "change"
                            (lambda (element)
                              (send-put-request url input-type
                                                (ps:@ element parent-node dataset name)
                                                (ps:@ element value)))))

    (defun setup-event-listeners ()
      (setup-change-event-listener "num-input" "/heat" "number")
      (setup-change-event-listener "slider light" "/light" "slider")

      (setup-event-listener "switch" "click"
                            (lambda (button)
                              (button-event-listener
                               (ps:chain button (query-selector "svg.on"))
                               (ps:chain button (query-selector "svg.off"))
                               (ps:@ button name)
                               (ps:@ button parent-node dataset name)
                               (ps:@ button dataset)))))

    (defun initialize-switch-states ()
      (let ((elements (ps:chain document (get-elements-by-class-name "switch"))))
        (loop for element across elements
              do (set-display-style
                  (ps:chain element (query-selector "svg.on"))
                  (ps:chain element (query-selector "svg.off"))
                  (ps:@ element dataset state)))))

    (initialize-switch-states)
    (setup-event-listeners)))

(defparameter stylesheet.css
  (lass:compile-and-write
   '(:root
     :--orange_3 "#ff7800"
     :--red_3 "#e01b24"
     :--light_2 "#f6f5f4"
     :--light_3 "#deddda"
     :--dark_3 "#3d3846"
     :--dark_5 "#000"
     :--accent-color "var(--orange_3)")

   '(::selection
     :background-color "color-mix(in hsl, var(--accent-color) 50%, transparent))")

   '(:media "(prefers-color-scheme: no-preference)"
     (::root
      :--background "var(--light_2)"
      :--background-2 "var(--light_3)"
      :--foreground "var(--dark_3)"
      ))

   '(:media "(prefers-color-scheme: light)"
     (::root
      :--background "var(--light_2)"
      :--background-2 "var(--light_3)"
      :--foreground "var(--dark_3)"
      ))

   '(:media "(prefers-color-scheme: dark)"
     (::root
      :--background "var(--dark_5)"
      :--background-2 "var(--dark_3)"
      :--foreground "var(--light_2)"
      ))

   '(body
     :border 0
     :margin 0
     :padding 0
     :font-family "system-ui"
     :padding-block "1rem"
     :width "100%"
     :display "flex"
     :flex-direction "column"
     :align-items "center"
     :background-color "var(--background)"
     :color "var(--foreground)"
     :overflow-x "hidden"
     :accent-color "var(--accent-color)"
     :touch-action "manipulation"
     )

   '(.controls
     :max-width "100vw"
     :height "100%"
     :display "flex"
     :flex-direction "column"
     :align-items "center")

   '(.control-group
     :width "90%"
     :min-width "25vw"
     :display "flex"
     :flex-direction "column"
     :border "1rem solid var(--background-2)"
     :border-radius "0.5rem"
     :background-color "var(--background-2)"
     :margin-bottom "1rem")

   '((.control-group > .label)
     :margin-bottom "1rem"
     :text-decoration "underline"
     :text-underline-position "below"
     :text-decoration-color "var(--accent-color)")

   '((:and .control-group :last-child)
     :margin-bottom 0)

   '(.control
     :width "100%"
     :display "flex"
     :align-items "center"
     :margin-bottom "1rem")

   '((.control > .label)
     :width "30%"
     :text-align "right")

   '(.switch
     :width "2.5rem"
     :height "2.5rem"
     :align-items "center"
     :justify-content "center"
     :margin "auto"
     :background "none"
     :border "none"
     :cursor "pointer"
     :outline "none"
     :white-space-collapse "collapse")

   '((.switch > svg)
     :color "var(--foreground)")

   '((.switch > .off)
     :display "none")

   '((:and .switch :disabled)
     :opacity "50%")

   '(.slider
     :width "50%"
     :outline "none")

   '((:and .slider :hover)
     :cursor "grab")

   '((:and .slider :active)
     :cursor "grabbing")

   '((:and .slider :disabled)
     :accent-color "var(--foreground)")

   '(.current-temperature
     :width "20%"
     :text-align "center")

   '(.num-input
     :width "50%"
     :border "none"
     :background "none"
     :border-bottom "0.2em solid var(--accent-color)"
     :border-radius 0
     :color "var(--foreground)"
     :outline "none"
     :font-size "1rem")

   '((:and .num-input :focus)
     :border-color "var(--background-2)"
     :outline "0.2em solid var(--accent-color)")

   '((:and .num-input :invalid)
     :text-decoration-line "underline"
     :text-decoration-style "wavy"
     :text-decoration-color "var(--red_3)")

   ))

(defun controls ()
  (spinneret:with-html-string
    (:doctype)
    (:head
     (:meta :name "viewport" :content "width=device-width, initial-scale=1")
     (:title "Hjem")
     (:style (:raw stylesheet.css)))
    (:body
     (:div :class "controls"
      (:raw
       (render-control-group
        "LIGHT"
        (render-light "couloir")
        (render-light "entree")
        (render-light "cuisine")
        (render-light "manger")
        (render-light "chonk")
        (render-light "chambre"))
       (render-control-group
        "HEAT"
        (render-heater "bureau" t)
        (render-heater "chambre")
        (render-heater "salon")
        (render-heater "rachel"))))
     (:script (:raw main.js)))))

;; (format t "~A" (controls))

(defun handle-put (env)
  ;; TODO: err status code when MQTT send fails
  (let* ((stream (getf env :raw-body))
         (payload (read-line stream)))

    (format t "=> payload: ~A~%" payload)

    (when (equal (getf env :request-uri) "/heat")
      (handle-heat payload))

    (when (equal (getf env :request-uri) "/light")
      (handle-light payload)))

  (list 200 '(:content-type "text/html") '("")))

(defun response (env)
  ;; (format t "env: ~A~%" env)

  ;; Response:
  ;; [status code] [headers (plist)] [body (strings / vector / pathname)]

  (case (getf env :request-method)
    (:PUT (handle-put env))
    (t (list 200 '(:content-type "text/html") (list (controls))))))

(format t "Starting webserver~%")

(defvar *handler*
  (clack:clackup 'response :address "0.0.0.0"))

(format t "Hit enter to stop the server~%")
(read-line)

(clack:stop *handler*)
(format t "Stopped~%")

#|
;; How to install deps and run
sbcl --script /home/john/hal9k/web.lisp

|#

;; TODO:
;; - add "force" heater button
;; - find out what crashes the webapp
;; - add feedback to thermostat selector
