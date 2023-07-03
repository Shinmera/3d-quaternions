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
   #:q<-
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
   #:nq+*
   #:q.
   #:qlength2
   #:qlength
   #:qunit
   #:qunit*
   #:nqunit
   #:nqunit*
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
   #:qi
   #:qj
   #:qk
   #:qr
   #:dquat
   #:dqcopy
   #:dquat-p
   #:qreal
   #:qdual))
