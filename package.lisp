#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.quaternion
  (:use #:cl #:org.shirakumo.flare.vector #:org.shirakumo.flare.matrix)
  (:import-from #:org.shirakumo.flare.vector
                #:define-vecx-accessor #:define-ofun #:ensure-float #:*float-type*
                #:ensure-float-param #:intern* #:defsetf* #:%vx3 #:%vy3 #:%vz3)
  (:import-from #:org.shirakumo.flare.matrix
                #:*eps* #:~= #:~/=)
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
