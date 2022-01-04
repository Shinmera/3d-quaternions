#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.flare.quaternion.test
  (:use #:cl #:parachute #:3d-vectors #:3d-matrices #:3d-quaternions))
(in-package #:org.shirakumo.flare.quaternion.test)

(define-test 3d-quaternions)

(define-test struct
  :parent 3d-quaternions
  (of-type quat (quat))
  (of-type quat (quat 0 0 0 1))
  (of-type dquat (dquat))
  (of-type dquat (dquat 0 0 0 1 0 0 0 1))
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

(define-test comparators
  :parent 3d-vectors
  :depends-on (struct))

(define-test arithmetic
  :parent 3d-vectors
  :depends-on (comparators))

(define-test math
  :parent 3d-vectors
  :depends-on (arithmetic))
