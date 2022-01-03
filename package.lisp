#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.quaternion
  (:use #:cl)
  (:import-from #:org.shirakumo.flare.vector
                #:define-vecx-accessor #:define-ofun #:ensure-float #:*float-type*)
  ;; ops.lisp
  (:export
   )
  ;; struct.lisp
  (:export
   #:quat
   #:qcopy
   #:quat-p
   #:qx
   #:qy
   #:qz
   #:qw
   #:dquat
   #:dqcopy
   #:dquat-p
   #:dx
   #:dy
   #:dz
   #:dw))
