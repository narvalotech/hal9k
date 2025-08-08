(defsystem "home"
  :description "home: manages the lights and space heaters"
  :version "0.0.1"
  :author "Jonathan Rico <jonathan@rico.live>"
  :depends-on ("cl-mqtt" "cl-json" "local-time" "slynk" "trivial-timer" "bordeaux-threads")
  :components ((:file "home"))
  :in-order-to ((test-op (test-op "home/tests"))))

(defsystem "home/tests"
  :depends-on ("home" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (o s) (symbol-call :home/tests :run-all)))
