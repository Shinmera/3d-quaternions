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
   #:dqsetf
   #:qsetf
   #:qfrom-angle
   #:qtowards
   #:qaxis
   #:qangle
   #:q=
   #:q/=
   #:qequal
   #:q+
   #:nq+
   #:q-
   #:nq-
   #:q*
   #:nq*
   #:q/
   #:nq/
   #:q.
   #:qlength2
   #:qlength
   #:qunit
   #:nqunit
   #:qconjugate
   #:qinv
   #:q*v
   #:qmix
   #:qnlerp
   #:qexpt
   #:nqexpt
   #:qslerp
   #:qlookat
   #:qmat3
   #:qmat4
   #:qfrom-mat
   #:qposition
   #:qfrom-position)
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
   #:qreal
   #:qdual))
