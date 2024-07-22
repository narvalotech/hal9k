(ql:quickload :parenscript)
(ql:quickload :clack)
(ql:quickload :spinneret)

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

(defun render-slider (name)
  (spinneret:with-html-string
    (:li (:input :type :range
                 :class "slider"
                 :name name
                 :min 0 :max 255 :step 1))))

(defun render-toggle (name)
  (spinneret:with-html-string
    (:li (:button
          :class "toggle-button"
          :value name name))))

(defparameter jsmain
  (ps:ps
    (defun send-put-request (url action value)
      (let ((data (ps:create :action action :name value)))
        (ps:chain (fetch url
                         (ps:create :method "PUT"
                                    :headers (ps:create "Content-Type" "application/json")
                                    :body (ps:chain *json* (stringify data))))
                  (then (lambda (response) (ps:@ response json)))
                  (then (lambda (data) (ps:@ console (log data)))))))

    ;; note: use "input" for events on value change
    (defun setup-slider-event-listeners ()
      (let ((sliders (ps:chain document (get-elements-by-class-name "slider"))))
        (loop for slider across sliders
              do (ps:chain slider (add-event-listener
                                   "change"
                                   (lambda () (send-put-request "/input" "slider" (ps:@ slider value))))))))

    (defun setup-button-event-listeners ()
      (let ((buttons (ps:chain document (get-elements-by-class-name "toggle-button"))))
        (loop for button across buttons
              do (ps:chain button (add-event-listener
                                   "click"
                                   (lambda () (send-put-request "/input" "toggle" (ps:@ button value))))))))

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
       (render-slider "chambre")
       (render-toggle "chambre")
       (render-toggle "couloir")))
     (:script (:raw jsmain)))))

(defun response (env)
  ;; (declare (ignore env))
  (format t "env: ~A~%" env)
  ;; (break)
  (when (equal (getf env :request-method) :PUT)
    (let* ((stream (getf env :raw-body)))
      (format t "=> payload: ~A~%"
              (read-line stream))))

  ;; Response:
  ;; [status code] [headers (plist)] [body (strings / vector / pathname)]
  (list 200 '(:content-type "text/html") (list (controls))))

(defvar *handler*
  (clack:clackup
   'response))

(clack:stop *handler*)
