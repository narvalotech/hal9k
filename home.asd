(defsystem "home"
  :description "home: manages the lights and space heaters"
  :version "0.0.1"
  :author "Jonathan Rico <jonathan@rico.live>"
  :depends-on ("cl-mqtt" "cl-json" "local-time" "slynk")
  :components ((:file "home")))
