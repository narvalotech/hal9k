(mqtt-connect-to-broker "192.168.10.175" 1883 #'test-app-callback)
(subscribe *broker* "#")
(disconnect *broker*)

;; Scratchpad
;; Fetch the problematic data stream from the lisp inspector
;; -> M-RET (or copy-to-repl)
(defparameter *rec1* *)
(defparameter *rec2* *)

(coerce *rec1* 'list)
(untrace mqtt-parse-packets)
(untrace mqtt-parse-packet)

(length (append (coerce *rec1* 'list) (coerce *rec2* 'list)))
(mqtt-next-packet (coerce *rec1* 'list))
(mqtt-parse-packets (coerce *rec1* 'list))
(mqtt-parse-packets (coerce *rec2* 'list))

(defun hex->list (str)
  "Convert a string of hex bytes into a list of integers"
  (loop for i from 0 below (length str) by 2
        collect (parse-integer (subseq str i (min (+ i 2) (length str)))
                               :radix 16)))

(let* ((packets (mqtt-parse-packets *z2m-info*))
       (packet (first packets))
      )
  (json:decode-json-from-string
  (ascii->string (getf (cdr packet) :payload))))

(let ((socket (getf *broker* :socket))
      (stream (getf *broker* :stream)))

  (send-packet socket stream (subseq *z2m-info* 0 12))
  ;; apparently not enough to cause problems.
  ;; We may need to mess with the broker <-> client connection
  (sleep 1)
  (send-packet socket stream (subseq *z2m-info* 12)))

(with-fn-shadow ('publish #'fake-publish)
  (app-callback nil *rec4*))

(mqtt:with-broker ("192.168.10.175" 1883 *broker* :client-id-str "agent")
    (publish broker "z2m/light-manger/set/state" "ON")
    (sleep 1)
    (publish broker "z2m/light-manger/set/state" "OFF")
    )

(mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
  (set-brightness broker "z2m/light-chambre" 250))

(mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
    (publish broker "z2m/light-chambre/set/state" "OFF"))

(mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
    (publish broker "z2m/light-chambre/set/state" "ON"))

(mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
    (mqtt:publish broker "z2m/force-bureau/set/state" "on"))

(mqtt:with-broker ("192.168.10.175" 1883 broker :client-id-str "agent")
    (mqtt:publish broker "z2m/enable-bureau/set/state" "off"))

;; -------------------

(mqtt:connect-to-broker "192.168.10.175" 1883 #'app-callback)
(progn (format t "DO-IT~%") (mqtt:subscribe *broker* "#"))
(progn (mqtt:disconnect *broker*) (setf *broker* nil))

(mqtt:publish *broker* "z2m/light-chambre/set/state" "ON")

(defun slime-connected-p ()
  (and (boundp '*slime-connection*)
       *slime-connection*))

(defun custom-debugger-hook (condition hook)
  (if (slime-connected-p)
      (invoke-debugger condition)
      (progn
        (format t "Error discarded: ~A~%" condition)
        (values))))

(setf *debugger-hook* #'custom-debugger-hook)
