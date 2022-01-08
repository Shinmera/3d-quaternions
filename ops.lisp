#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.quaternion)

(defmacro qsetf (&environment env quat x y z w)
  (let ((q (gensym "QUAT")))
    `(let ((,q ,quat))
       (psetf (%vx3 ,q) ,(ensure-float-param x env)
              (%vy3 ,q) ,(ensure-float-param y env)
              (%vz3 ,q) ,(ensure-float-param z env)
              (%qw ,q) ,(ensure-float-param w env))
       ,q)))

(defmacro dqsetf (quat real dual)
  (let ((q (gensym "QUAT")))
    `(let ((,q ,quat))
       (psetf (qreal ,q) ,real
              (qdual ,q) ,dual)
       ,q)))

(define-ofun qfrom-angle (axis angle)
  (let ((s (sin (* 0.5 (ensure-float angle)))))
    (quat (* s (vx3 axis))
          (* s (vy3 axis))
          (* s (vz3 axis))
          (cos (* 0.5 angle)))))

(define-ofun qtowards (from to)
  (let ((from (vunit from))
        (to (vunit to)))
    (cond ((v= from to)
           (quat))
          ((v= from (v* to -1.0))
           (let* ((ortho (cond ((< (abs (vy3 from)) (abs (vx3 from)))
                                +vy+)
                               ((and (< (abs (vz3 from)) (abs (vy3 from)))
                                     (< (abs (vz3 from)) (abs (vx3 from))))
                                +vz+)
                               (T
                                +vx+)))
                  (axis (nvunit (vc from ortho))))
             (quat (vx3 axis) (vy3 axis) (vz3 axis) 0.0)))
          (T
           (let* ((half (nvunit (v+ from to)))
                  (axis (vc from half)))
             (quat (vx3 axis) (vy3 axis) (vz3 axis) (v. from half)))))))

(define-ofun qaxis (quat)
  (vunit quat))

(define-ofun qangle (quat)
  (* 2.0 (the single-float (acos (qw quat)))))

(defmacro %2quat-op (name a b combination qred dqred &optional 2q-override)
  (let ((qred (if (listp qred) qred (list qred)))
        (dqred (if (listp dqred) dqred (list dqred)))
        (ag (gensym "A")) (bg (gensym "B")))
    `(etypecase ,a
       (real (let ((,ag (ensure-float ,a)))
               (etypecase ,b
                 (quat (,@qred (,combination ,ag (qx ,b)) (,combination ,ag (qy ,b)) (,combination ,ag (qz ,b)) (,combination ,ag (qw ,b))))
                 (dquat (,@dqred (,name ,a (qreal ,b)) (,name ,a (qdual ,b)))))))
       (quat (etypecase ,b
               (real (let ((,bg (ensure-float ,b)))
                       (,@qred (,combination (qx ,a) ,bg) (,combination (qy ,a) ,bg) (,combination (qz ,a) ,bg) (,combination (qw ,a) ,bg))))
               (quat ,(if 2q-override
                          `(,2q-override ,a ,b ,qred)
                          `(,@qred (,combination (qx ,a) (qx ,b)) (,combination (qy ,a) (qy ,b)) (,combination (qz ,a) (qz ,b)) (,combination (qw ,a) (qw ,b)))))))
       (dquat (etypecase ,b
                (real (,@dqred (,name (qreal ,a) ,b) (,name (qdual ,a) ,b)))
                (dquat ,(if 2q-override
                            `(,2q-override ,a ,b ,dqred)
                            `(,@dqred (,name (qreal ,a) (qreal ,b)) (,name (qdual ,a) (qdual ,b))))))))))

(defmacro define-quatcomp (name op &optional (bundle 'and))
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (declaim (ftype (function ((or quat real) (or quat real)) boolean) ,2quat-name))
       (declaim (ftype (function ((or quat real) &rest (or quat real)) boolean) ,name))
       (declaim (inline ,name ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op ,2quat-name a b ,op ,bundle ,bundle))
       (define-ofun ,name (val &rest vals)
         (loop for prev = val then next
               for next in vals
               always (,2quat-name prev next)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 T)
           (1 `(,',2quat-name ,val ,(first vals)))
           (T `(and ,@(loop for prev = val then next
                            for next in vals
                            collect `(,',2quat-name ,prev ,next)))))))))

(define-quatcomp q= ~=)
(define-quatcomp q/= ~/= or)

(define-ofun qequal (a b)
  (etypecase a
    (quat (or (q= a b)
              (and (< (abs (+ (qx a) (qx b))) *eps*)
                   (< (abs (+ (qy a) (qy b))) *eps*)
                   (< (abs (+ (qz a) (qz b))) *eps*)
                   (< (abs (+ (qw a) (qw b))) *eps*))))
    (dquat (and (qequal (qreal a) (qreal b))
                (q= (qdual a) (qdual b))))))

(defmacro define-nquatop (name op &optional 2q-override)
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (declaim (inline ,name ,2quat-name))
       (declaim (ftype (function (quat &rest (or quat real)) quat) ,name))
       (declaim (ftype (function (quat (or quat real)) quat) ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op ,2quat-name a b ,op (qsetf a) (dqsetf a) ,2q-override))
       (define-ofun ,name (val &rest vals)
         (if vals
             (loop for v in vals
                   do (,2quat-name val v)
                   finally (return val))
             (vapplyf val ,op)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vapplyf ,val ,',op))
           (1 `(,',2quat-name ,val ,(first vals)))
           (T `(,',name (,',2quat-name ,val ,(first vals)) ,@(rest vals))))))))

(defmacro define-quatop (name nname op &optional 2q-override)
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (define-nquatop ,nname ,op ,2q-override)
       (declaim (inline ,name ,2quat-name))
       (declaim (ftype (function ((or quat real) &rest (or quat real)) quat) ,name))
       (declaim (ftype (function ((or quat real) (or quat real)) quat) ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op ,2quat-name a b ,op quat dquat ,2q-override))
       (define-ofun ,name (val &rest vals)
         (cond ((cdr vals)
                (apply #',nname (,2quat-name val (first vals)) (rest vals)))
               (vals (,2quat-name val (first vals)))
               (T (vapply val ,op))))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(vapply ,val ,',op))
           (1 `(,',2quat-name ,val ,(first vals)))
           (T `(,',nname (,',2quat-name ,val ,(first vals)) ,@(rest vals))))))))

(define-quatop q+ nq+ +)
(define-quatop q- nq- -)
(macrolet ((2-exp (a b red)
             (case (car red)
               ((qsetf quat)
                `(,@red
                  (+ (+ (* (qx ,b) (qw ,a))) (+ (* (qy ,b) (qz ,a))) (- (* (qz ,b) (qy ,a))) (+ (* (qw ,b) (qx ,a))))
                  (+ (- (* (qx ,b) (qx ,a))) (+ (* (qy ,b) (qw ,a))) (+ (* (qz ,b) (qx ,a))) (+ (* (qw ,b) (qy ,a))))
                  (+ (+ (* (qx ,b) (qy ,a))) (- (* (qy ,b) (qx ,a))) (+ (* (qz ,b) (qw ,a))) (+ (* (qw ,b) (qz ,a))))
                  (+ (- (* (qx ,b) (qz ,a))) (- (* (qy ,b) (qy ,a))) (- (* (qz ,b) (qz ,a))) (+ (* (qw ,b) (qw ,a))))))
               (dquat
                `(let ((,a (qunit ,a))
                       (,b (qunit ,b)))
                   (dquat
                    (q* (qreal ,a) (qreal ,b))
                    (nq+ (q* (qreal ,a) (qdual ,b))
                         (q* (qdual ,a) (qreal ,b))))))
               (dqsetf
                `(let ((,a (nqunit ,a))
                       (,b (qunit ,b)))
                   (dqsetf ,a
                           (q* (qreal ,a) (qreal ,b))
                           (nq+ (q* (qreal ,a) (qdual ,b))
                                (q* (qdual ,a) (qreal ,b)))))))))
  (define-quatop q* nq* * 2-exp))
(define-quatop q/ nq/ /)

(declaim (inline q.))
(define-ofun q. (a b)
  (flet ((thunk (a b)
           (+ (* (qx a) (qx b))
              (* (qy a) (qy b))
              (* (qz a) (qz b))
              (* (qw a) (qw b)))))
    (etypecase a
      (quat
       (thunk a b))
      (dquat
       (thunk (qreal a) (qreal b))))))

(declaim (inline qlength2))
(declaim (ftype (function (quat) (#.*float-type* 0.0)) qlength2))
(define-ofun qlength2 (a)
  (q. a a))

(declaim (inline qlength))
(define-ofun qlength (a)
  (sqrt (qlength2 a)))

(declaim (inline qunit))
(define-ofun qunit (a)
  (etypecase a
    (quat
     (q/ a (qlength a)))
    (dquat
     (let ((mag (/ (qlength (qreal a)))))
       (dquat (q* (qreal a) mag)
              (q* (qdual a) mag))))))

(declaim (inline nqunit))
(define-ofun nqunit (a)
  (etypecase a
    (quat
     (nq/ a (qlength a)))
    (dquat
     (let ((mag (/ (qlength (qreal a)))))
       (nq* (qreal a) mag)
       (nq* (qdual a) mag)
       a))))

(declaim (inline qconjugate))
(define-ofun qconjugate (a)
  (flet ((thunk (a)
           (quat (- (qx a)) (- (qy a)) (- (qz a)) (qw a))))
    (etypecase a
      (quat
       (thunk a))
      (dquat
       (dquat (thunk (qreal a)) (thunk (qdual a)))))))

(define-ofun qinv (a)
  (let ((div (/ -1.0 (qlength2 a))))
    (quat (* (qx a) div) (* (qy a) div) (* (qz a) div) (* (qw a) (- div)))))

(define-ofun q*v (q v)
  (etypecase q
    (quat
     (let ((qw2 (* (qw q) (qw q)))
           (2qw (* 2.0 (qw q)))
           (q.q (v. q q))
           (q.v (v. q v))
           (c (vc q v)))
       (vec (+ (* 2.0 (qx q) q.v) (* (vx v) (- qw2 q.q)) (* 2qw (vx c)))
            (+ (* 2.0 (qy q) q.v) (* (vy v) (- qw2 q.q)) (* 2qw (vx c)))
            (+ (* 2.0 (qz q) q.v) (* (vz v) (- qw2 q.q)) (* 2qw (vx c))))))
    (dquat
     (let ((d (nq* (qconjugate (qreal q)) (qdual q) 2.0)))
       (nv+ (q*v (qreal q) v) d)))))

(define-ofun qmix (from to x)
  (nq+ (q* from (- 1.0 (ensure-float x)))
       (q* to x)))

(define-ofun qnlerp (from to x)
  (nqunit (nq+ (nq* (q- from to) x) from)))

(define-ofun qexpt (quat exp)
  (let* ((f (* (ensure-float exp)
               (acos (the (#.*float-type* -1.0 +1.0) (qw quat)))))
         (axis (vunit quat))
         (cos/2 (cos f))
         (sin/2 (sin f)))
    (quat (* (qx axis) sin/2)
          (* (qy axis) sin/2)
          (* (qz axis) sin/2)
          cos/2)))

(define-ofun nqexpt (quat exp)
  (let* ((f (* (ensure-float exp)
               (acos (the (#.*float-type* -1.0 +1.0) (qw quat)))))
         (axis (vunit quat))
         (cos/2 (cos f))
         (sin/2 (sin f)))
    (qsetf quat
           (* (qx axis) sin/2)
           (* (qy axis) sin/2)
           (* (qz axis) sin/2)
           cos/2)))

(define-ofun qslerp (from to x)
  (if (< (- 1.0 *eps*) (abs (q. from to)))
      (qnlerp from to x)
      (nqunit (nq* (nqexpt (nq* (qinv from) to) x) from))))

(define-ofun qlookat (dir up)
  (let* ((f (vunit dir))
         (u (vunit up))
         (r (vc f u))
         (u (vc f r))
         (world->object (qtowards +vz+ f))
         (object-up (q*v world->object +vy+))
         (up->up (qtowards object-up u)))
    (nvunit (nq* world->object up->up))))

(define-ofun qmat3 (quat)
  (let* ((x (qx quat)) (y (qy quat)) (z (qz quat)) (w (qw quat))
         (xx (* x x)) (xy (* x y)) (xz (* x z)) (xw (* x w))
         (yy (* y y)) (yz (* y z)) (yw (* y w))
         (zz (* z z)) (zw (* z w)))
    (mat3 (list (- 1 (* 2 (+ yy zz))) (* 2 (- xy zw)) (* 2 (+ xz yw))
                (* 2 (+ xy zw)) (- 1 (* 2 (+ xx zz))) (* 2 (- yz xw))
                (* 2 (- xz yw)) (* 2 (+ yz xw)) (- 1 (* 2 (+ xx yy)))))))

(define-ofun qmat4 (quat)
  (let* ((x (qx quat)) (y (qy quat)) (z (qz quat)) (w (qw quat))
         (xx (* x x)) (xy (* x y)) (xz (* x z)) (xw (* x w))
         (yy (* y y)) (yz (* y z)) (yw (* y w))
         (zz (* z z)) (zw (* z w)))
    (mat4 (list (- 1 (* 2 (+ yy zz))) (* 2 (- xy zw)) (* 2 (+ xz yw)) 0.0
                (* 2 (+ xy zw)) (- 1 (* 2 (+ xx zz))) (* 2 (- yz xw)) 0.0
                (* 2 (- xz yw)) (* 2 (+ yz xw)) (- 1 (* 2 (+ xx yy))) 0.0
                0.0             0.0             0.0                   1.0))))

(defun qfrom-mat (mat)
  (macrolet ((stub ()
               `(let ((w (* 0.5 (sqrt (max 0.0 (+ 1.0 (+ (m 0 0)) (+ (m 1 1)) (+ (m 2 2)))))))
                      (x (* 0.5 (sqrt (max 0.0 (+ 1.0 (+ (m 0 0)) (- (m 1 1)) (- (m 2 2)))))))
                      (y (* 0.5 (sqrt (max 0.0 (+ 1.0 (- (m 0 0)) (+ (m 1 1)) (- (m 2 2)))))))
                      (z (* 0.5 (sqrt (max 0.0 (+ 1.0 (- (m 0 0)) (- (m 1 1)) (+ (m 2 2))))))))
                  (quat (float-sign (- (m 2 1) (m 1 2)) x)
                        (float-sign (- (m 0 2) (m 2 0)) y)
                        (float-sign (- (m 1 0) (m 0 1)) z)
                        w))))
    (etypecase mat
      (mat3
       (with-fast-matref (m mat 3)
         (stub)))
      (mat4
       (with-fast-matref (m mat 4)
         (stub))))))

(defun qposition (dquat)
  (vxyz (nq* (qconjugate (qreal dquat)) (qdual dquat) 2.0)))

(defun qfrom-position (quat vec)
  (dquat quat (q* quat (quat (vx vec) (vy vec) (vz vec) 0) 0.5)))
