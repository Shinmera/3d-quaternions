#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem 3d-quaternions
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A utility library implementing quaternion and dual-quaternion functionality."
  :homepage "https://Shinmera.github.io/3d-quaternions/"
  :bug-tracker "https://github.com/Shinmera/3d-quaternions/issues"
  :source-control (:git "https://github.com/Shinmera/3d-quaternions.git")
  :serial T
  :components ((:file "package")
               (:file "struct")
               (:file "ops")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :3d-vectors
               :3d-matrices)
  :in-order-to ((asdf:test-op (asdf:test-op :3d-quaternions-test))))
