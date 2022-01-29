#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.quaternion.test
  (:use #:cl #:parachute
        #:org.shirakumo.flare.vector
        #:org.shirakumo.flare.matrix
        #:org.shirakumo.flare.quaternion)
  (:import-from #:org.shirakumo.flare.matrix #:~=))
(in-package #:org.shirakumo.flare.quaternion.test)

(define-test 3d-quaternions)

(define-test struct
  :parent 3d-quaternions
  (of-type quat (quat))
  (of-type quat (quat 0 0 0 1))
  (of-type dquat (dquat))
  (of-type dquat (dquat (quat 0 0 0 1) (quat 0 0 0 1)))
  (true (quat-p (quat)))
  (true (dquat-p (dquat)))
  (is = 0 (qx (quat)))
  (is = 0 (qy (quat)))
  (is = 0 (qz (quat)))
  (is = 1 (qw (quat)))
  (is = 1 (qx (quat 1 2 3 4)))
  (is = 2 (qy (quat 1 2 3 4)))
  (is = 3 (qz (quat 1 2 3 4)))
  (is = 4 (qw (quat 1 2 3 4))))

(define-test misc
  :parent 3d-quaternions
  :depends-on (struct)
  (is q= (quat) (quat))
  (is q= (quat 1 2 3 4) (quat 1 2 3 4))
  (isnt q= (quat 1 2 3 4) (quat 1 2 3 5))
  (is q/= (quat 1 2 3 4) (quat 1 2 3 5))
  (isnt q/= (quat) (quat))
  (is q= (quat 1 2 3 4) (qsetf (quat) 1 2 3 4))
  (is qequal (qfrom-angle +vx+ (+ PI)) (qfrom-angle +vx+ (- PI)))
  (is v= +vy+ (qaxis (qfrom-angle +vy+ PI)))
  (is ~= (coerce PI 'single-float) (qangle (qfrom-angle +vy+ PI)))
  (is q= (qfrom-angle +vy+ PI) (qfrom-mat (qmat4 (qfrom-angle +vy+ PI))))
  (is q= (qfrom-angle +vx+ (/ PI 2)) (qfrom-mat (qmat4 (qfrom-angle +vx+ (/ PI 2)))))
  (is qequal (qfrom-angle +vy+ PI) (qfrom-mat (mrotation +vy+ PI))))

(define-test arithmetic
  :parent 3d-quaternions
  :depends-on (comparators))

(define-test math
  :parent 3d-quaternions
  :depends-on (arithmetic))
