(asdf:defsystem 3d-quaternions
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing quaternion and dual-quaternion functionality."
  :homepage "https://shinmera.com/docs/3d-quaternions/"
  :bug-tracker "https://shinmera.com/project/3d-quaternions/issues"
  :source-control (:git "https://shinmera.com/project/3d-quaternions.git")
  :serial T
  :components ((:file "package")
               (:file "struct")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors
               :3d-matrices)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-quaternions-test))))
