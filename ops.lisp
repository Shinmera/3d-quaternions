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

(defmacro qapply (&environment env quat op &optional x y z w)
  (let ((q (gensym "QUAT")))
    `(let ((,q ,quat))
       (quat (,op (qx ,q) ,@(when x (list (ensure-float-param x env))))
             (,op (qy ,q) ,@(when y (list (ensure-float-param y env))))
             (,op (qz ,q) ,@(when z (list (ensure-float-param z env))))
             (,op (qw ,q) ,@(when w (list (ensure-float-param w env))))))))

(defmacro qapplyf (&environment env quat op &optional x y z w)
  (let ((q (gensym "QUAT")))
    `(let ((,q ,quat))
       (qsetf ,q
              (,op (qx ,q) ,@(when x (list (ensure-float-param x env))))
              (,op (qy ,q) ,@(when y (list (ensure-float-param y env))))
              (,op (qz ,q) ,@(when z (list (ensure-float-param z env))))
              (,op (qw ,q) ,@(when w (list (ensure-float-param w env)))))
       ,q)))

(define-ofun q<- (target source)
  (setf (%vx3 target) (%vx3 source))
  (setf (%vy3 target) (%vy3 source))
  (setf (%vz3 target) (%vz3 source))
  (setf (%qw target) (%qw source))
  target)

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
             (qapplyf val ,op)))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(qapplyf ,val ,',op))
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
               (T (qapply val ,op))))
       (define-compiler-macro ,name (val &rest vals)
         (case (length vals)
           (0 `(qapply ,val ,',op))
           (1 `(,',2quat-name ,val ,(first vals)))
           (T `(,',nname (,',2quat-name ,val ,(first vals)) ,@(rest vals))))))))

(define-quatop q+ nq+ +)
(define-quatop q- nq- -)
(macrolet ((2-exp (a b red)
             (case (car red)
               ((qsetf quat)
                `(,@red
                  (+ (+ (* (qx a) (qw b))) (+ (* (qy a) (qz b))) (- (* (qz a) (qy b))) (+ (* (qw a) (qx b))))
                  (+ (- (* (qx a) (qz b))) (+ (* (qy a) (qw b))) (+ (* (qz a) (qx b))) (+ (* (qw a) (qy b))))
                  (+ (+ (* (qx a) (qy b))) (- (* (qy a) (qx b))) (+ (* (qz a) (qw b))) (+ (* (qw a) (qz b))))
                  (+ (- (* (qx a) (qx b))) (- (* (qy a) (qy b))) (- (* (qz a) (qz b))) (+ (* (qw a) (qw b))))))
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

(declaim (inline q*v))
(define-ofun q*v (q v)
  (etypecase q
    (quat
     (let* ((qw2 (* (qw q) (qw q)))
            (2qw (* 2.0 (qw q)))
            (q.q (- qw2 (v. q q)))
            (2q.v (* 2.0 (v. q v))))
       (vec (+ (* (vx3 q) 2q.v) (* (vx3 v) q.q) (* (- (* (vy3 q) (vz3 v)) (* (vz3 q) (vy3 v))) 2qw))
            (+ (* (vy3 q) 2q.v) (* (vy3 v) q.q) (* (- (* (vz3 q) (vx3 v)) (* (vx3 q) (vz3 v))) 2qw))
            (+ (* (vz3 q) 2q.v) (* (vz3 v) q.q) (* (- (* (vx3 q) (vy3 v)) (* (vy3 q) (vx3 v))) 2qw)))))
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
         (tx (* 2.0 x)) (ty (* 2.0 y)) (tz (* 2.0 z))
         (twx (* tx w)) (twy (* ty w)) (twz (* tz w))
         (txx (* tx x)) (txy (* tx x)) (txz (* tz x))
         (tyy (* ty y)) (tyz (* tz y)) (tzz (* tz z)))
    (mat (- 1.0 (+ tyy tzz)) (- txy twz) (+ txz twy)
         (+ txy twz) (- 1.0 (+ txx tzz)) (- tyz twx)
         (- txz twy) (+ tyz twx) (- 1.0 (+ txx tyy)))))

(define-ofun qmat4 (quat)
  (let* ((x (qx quat)) (y (qy quat)) (z (qz quat)) (w (qw quat))
         (tx (* 2.0 x)) (ty (* 2.0 y)) (tz (* 2.0 z))
         (twx (* tx w)) (twy (* ty w)) (twz (* tz w))
         (txx (* tx x)) (txy (* tx y)) (txz (* tz x))
         (tyy (* ty y)) (tyz (* tz y)) (tzz (* tz z)))
    (mat (- 1.0 (+ tyy tzz)) (- txy twz) (+ txz twy) 0.0
         (+ txy twz) (- 1.0 (+ txx tzz)) (- tyz twx) 0.0
         (- txz twy) (+ tyz twx) (- 1.0 (+ txx tyy)) 0.0
         0.0         0.0         0.0                 1.0)))

(defun qfrom-mat (mat)
  ;; From: https://d3cw3dd2w32x2b.cloudfront.net/wp-content/uploads/2015/01/matrix-to-quat.pdf
  (macrolet ((stub ()
               `(let ((tt 0.0))
                  (nq* (if (< (m 2 2) 0)
                           (cond ((< (m 1 1) (m 0 0))
                                  (setf tt (+ 1.0 (m 0 0) (- (m 1 1)) (- (m 2 2))))
                                  (quat tt (+ (m 0 1) (m 1 0)) (+ (m 2 0) (m 0 2)) (- (m 2 1) (m 1 2))))
                                 (T
                                  (setf tt (+ 1.0 (- (m 0 0)) (m 1 1) (- (m 2 2))))
                                  (quat (+ (m 0 1) (m 1 0)) tt (+ (m 1 2) (m 2 1)) (- (m 0 2) (m 2 0)))))
                           (cond ((< (m 0 0) (- (m 1 1)))
                                  (setf tt (+ 1.0 (- (m 0 0)) (- (m 1 1)) (+ (m 2 2))))
                                  (quat (+ (m 2 0) (m 0 2)) (+ (m 1 2) (m 2 1)) tt (- (m 1 0) (m 0 1))))
                                 (T
                                  (setf tt (+ 1.0 (+ (m 0 0)) (+ (m 1 1)) (+ (m 2 2))))
                                  (quat (- (m 2 1) (m 1 2)) (- (m 0 2) (m 2 0)) (- (m 1 0) (m 0 1)) tt))))
                       (/ 0.5 (sqrt tt))))))
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
