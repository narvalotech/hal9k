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

(defun render-toggle (name)
  (spinneret:with-html-string
    (:form :action "" :method "POST"
           (:input :type :hidden :name "action" :value "toggle")
           (:li (:button :name name :value "toggle" name)))))

(defun controls ()
  (spinneret:with-html-string
    (:doctype)
    (:head)
    (:body
     (:p "Lights")
     (:ul
      (:li (:input :type :range :name "intensity" :id "intensity" :min 0 :max 255 :step 1))
      (:raw
       (render-toggle "chambre")
       (render-toggle "couloir"))))))

(defun response (env)
  ;; (declare (ignore env))
  (format t "env: ~A~%" env)
  ;; (break)
  (when (equal (getf env :request-method) :POST)
    (let* ((stream (getf env :raw-body)))
      (format t "=> payload: ~A~%"
              (read-line stream))))

  ;; Response:
  ;; [status code] [headers (plist)] [body (strings / vector / pathname)]
  ;;
  ;; '(200 (:content-type "text/plain") ("Hello dude"))
  (list 200 '(:content-type "text/html") (list (controls)))

  )

(defvar *handler*
  (clack:clackup
   'response))

(clack:stop *handler*)
