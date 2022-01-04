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

(defun qfrom-angle (axis angle)
  (let ((s (sin (* 0.5 angle))))
    (quat (* s (vx3 axis))
          (* s (vy3 axis))
          (* s (vz3 axis))
          (cos (* 0.5 angle)))))

(defun qtowards (from to)
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

(defun qaxis (quat)
  (nvunit (vec (qx quat) (qy quat) (qz quat))))

(defun qangle (quat)
  (* 2.0 (acos (qw quat))))

(defmacro %2quat-op (a b combination red &rest 2q-override)
  (let ((red (if (listp red) red (list red)))
        (ag (gensym "A")) (bg (gensym "B")))
    `(etypecase ,a
       (real (let ((,ag (ensure-float ,a)))
               (etypecase ,b
                 (quat (,@red (,combination ,ag (qx ,b)) (,combination ,ag (qy ,b)) (,combination ,ag (qz ,b)) (,combination ,ag (qw ,b)))))))
       (quat (etypecase ,b
               (real (let ((,bg (ensure-float ,b)))
                       (,@red (,combination (qx ,a) ,bg) (,combination (qy ,a) ,bg) (,combination (qz ,a) ,bg) (,combination (qw ,a) ,bg))))
               (quat (,@red
                      ,@(or 2q-override
                            `((,combination (qx ,a) (qx ,b)) (,combination (qy ,a) (qy ,b)) (,combination (qz ,a) (qz ,b)) (,combination (qw ,a) (qw ,b)))))))))))

(defmacro define-quatcomp (name op &optional (bundle 'and))
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (declaim (ftype (function ((or quat real) (or quat real)) boolean) ,2quat-name))
       (declaim (ftype (function ((or quat real) &rest (or quat real)) boolean) ,name))
       (declaim (inline ,name ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op a b ,op ,bundle))
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
  (or (q= a b)
      (and (< (abs (+ (qx a) (qx b))) *eps*)
           (< (abs (+ (qy a) (qy b))) *eps*)
           (< (abs (+ (qz a) (qz b))) *eps*)
           (< (abs (+ (qw a) (qw b))) *eps*))))

(defmacro define-nquatop (name op &rest 2q-override)
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (declaim (inline ,name ,2quat-name))
       (declaim (ftype (function (quat &rest (or quat real)) quat) ,name))
       (declaim (ftype (function (quat (or quat real)) quat) ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op a b ,op (qsetf a) ,@2q-override))
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

(defmacro define-quatop (name nname op &rest 2q-override)
  (let ((2quat-name (intern* '2quat "-" name)))
    `(progn
       (define-nquatop ,nname ,op ,@2q-override)
       (declaim (inline ,name ,2quat-name))
       (declaim (ftype (function ((or quat real) &rest (or quat real)) quat) ,name))
       (declaim (ftype (function ((or quat real) (or quat real)) quat) ,2quat-name))
       (define-ofun ,2quat-name (a b)
         (%2quat-op a b ,op quat ,@2q-override))
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
(define-quatop q* nq* *
  (+ (+ (* (qx b) (qw a))) (+ (* (qy b) (qz a))) (- (* (qz b) (qy a))) (+ (* (qw b) (qx a))))
  (+ (- (* (qx b) (qx a))) (+ (* (qy b) (qw a))) (+ (* (qz b) (qx a))) (+ (* (qw b) (qy a))))
  (+ (+ (* (qx b) (qy a))) (- (* (qy b) (qx a))) (+ (* (qz b) (qw a))) (+ (* (qw b) (qz a))))
  (+ (- (* (qx b) (qz a))) (- (* (qy b) (qy a))) (- (* (qz b) (qz a))) (+ (* (qw b) (qw a)))))
(define-quatop q/ nq/ /)

(define-ofun q. (a b)
  (+ (* (qx a) (qx b))
     (* (qy a) (qy b))
     (* (qz a) (qz b))
     (* (qw a) (qw b))))

(declaim (ftype (function (quat) (#.*float-type* 0.0)) qlength2))
(define-ofun qlength2 (a)
  (q. a a))

(define-ofun qlength (a)
  (sqrt (qlength2 a)))

(define-ofun qunit (a)
  (q/ a (qlength a)))

(define-ofun nqunit (a)
  (nq/ a (qlength a)))

(define-ofun qconjugate (a)
  (quat (- (qx a)) (- (qy a)) (- (qz a)) (qw a)))

(define-ofun qinv (a)
  (let ((div (/ -1.0 (qlength2 a))))
    (quat (* (qx a) div) (* (qy a) div) (* (qz a) div) (* (qw a) (- div)))))

(define-ofun q*v (q v)
  (let ((qw2 (* (qw q) (qw q)))
        (2qw (* 2.0 (qw q)))
        (q.q (v. q q))
        (q.v (v. q v))
        (c (vc q v)))
    (vec (+ (* 2.0 (qx q) q.v) (* (vx v) (- qw2 q.q)) (* 2qw (vx c)))
         (+ (* 2.0 (qy q) q.v) (* (vy v) (- qw2 q.q)) (* 2qw (vx c)))
         (+ (* 2.0 (qz q) q.v) (* (vz v) (- qw2 q.q)) (* 2qw (vx c))))))

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
  (let ((r (q*v quat +vx+))
        (u (q*v quat +vy+))
        (f (q*v quat +vz+)))
    (mat3 (list (vx r) (vy r) (vz r)
                (vx u) (vy u) (vz u)
                (vx f) (vy f) (vz f)))))

(define-ofun qmat4 (quat)
  (let ((r (q*v quat +vx+))
        (u (q*v quat +vy+))
        (f (q*v quat +vz+)))
    (mat4 (list (vx r) (vy r) (vz r) 0
                (vx u) (vy u) (vz u) 0
                (vx f) (vy f) (vz f) 0
                0      0      0      1))))

(define-ofun qfrom-mat (mat4)
  (let* ((u (nvunit (vec (mcref4 mat4 1 0) (mcref4 mat4 1 1) (mcref4 mat4 1 2))))
         (f (nvunit (vec (mcref4 mat4 2 0) (mcref4 mat4 2 1) (mcref4 mat4 2 2))))
         (r (vc u f))
         (u (vc f r)))
    (qlookat f u)))
