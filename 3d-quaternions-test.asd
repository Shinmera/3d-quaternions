(asdf:defsystem 3d-quaternions-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the 3d-quaternions library"
  :homepage "https://shinmera.com/docs/3d-quaternions/"
  :bug-tracker "https://shinmera.com/project/3d-quaternions/issues"
  :source-control (:git "https://shinmera.com/project/3d-quaternions.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:3d-quaternions :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.flare.quaternion.test)))
