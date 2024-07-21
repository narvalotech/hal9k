(ql-dist:install-dist "http://dist.ultralisp.org/"
                               :prompt nil)

(ql:quickload '(:reblocks :reblocks-ui :find-port))
(ql:quickload :parenscript)
(ql:quickload :clack)
(ql:quickload :cl-who)
(ql:quickload :cl-fad)

(use-package :cl-who)
(use-package :parenscript)

(defun ps-ex-1 ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body (:h2 "Parenscript tutorial: 1st example")
            "Please click the link below." :br
            (:a :href "#"
                :onclick (ps (alert "Hello World"))
                "Hello World")))))

(defun ps-ex-1 ()
  (with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:script :type "text/javascript"
              (str (ps
                     (defun greeting-callback ()
                       (alert "Hello World")))))
     (:head (:title "Parenscript tutorial: 1st example"))
     (:body
      (:h2 "Parenscript tutorial: 2nd example")
      (:a :href "#" :onclick (ps (greeting-callback))
          "Hello World")))))

(defun response (env)
  (declare (ignore env))
  ;; (format t "env: ~A~%" env)

  ;; Response:
  ;; [status code] [headers (plist)] [body (strings / vector / pathname)]
  ;;
  ;; '(200 (:content-type "text/plain") ("Hello dude"))
  (list 200 '(:content-type "text/html") (list (ps-ex-1)))

  )

(defvar *handler*
  (clack:clackup
   'response))

(clack:stop *handler*)
