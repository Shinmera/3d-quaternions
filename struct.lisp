#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.quaternion)

(declaim (inline %quat))
(defstruct (quat (:conc-name NIL)
                 (:include vec3)
                 (:constructor %quat (%vx3 %vy3 %vz3 %qw))
                 (:copier qcopy)
                 (:predicate quat-p))
  (%qw (ensure-float 1) :type #.*float-type*))

(define-vecx-accessor qx %vx3)
(define-vecx-accessor qy %vy3)
(define-vecx-accessor qz %vz3)
(define-vecx-accessor qw %qw)

(declaim (inline quat))
(declaim (ftype (function (&optional real real real real) quat) quat))
(define-ofun quat (&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0))
  (%quat (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))

(define-compiler-macro quat (&environment env &optional (x 0) (y 0) (z 0) (w 1))
  `(%quat ,(ensure-float-param x env)
          ,(ensure-float-param y env)
          ,(ensure-float-param z env)
          ,(ensure-float-param w env)))

(defmethod print-object ((q quat) stream)
  (write (make-load-form q) :stream stream))

(defmethod make-load-form ((q quat) &optional env)
  (declare (ignore env))
  `(quat ,(qx q) ,(qy q) ,(qz q) ,(qw q)))

(defstruct (dquat (:conc-name NIL)
                  (:include quat)
                  (:constructor %dquat (%vx3 %vy3 %vz3 %qw %dx %dy %dz %dw))
                  (:copier dqcopy)
                  (:predicate dquat-p))
  (%dx (ensure-float 0) :type #.*float-type*)
  (%dy (ensure-float 0) :type #.*float-type*)
  (%dz (ensure-float 0) :type #.*float-type*)
  (%dw (ensure-float 1) :type #.*float-type*))

(define-vecx-accessor dx %dx)
(define-vecx-accessor dy %dy)
(define-vecx-accessor dz %dz)
(define-vecx-accessor dw %dw)

(declaim (inline dquat))
(declaim (ftype (function (&optional real real real real real real real real) dquat) dquat))
(define-ofun dquat (&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0) (dx 0.0) (dy 0.0) (dz 0.0) (dw 0.0))
  (%dquat (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)
          (ensure-float dx) (ensure-float dy) (ensure-float dz) (ensure-float dw)))

(define-compiler-macro dquat (&environment env &optional (x 0) (y 0) (z 0) (w 0) (dx 0) (dy 0) (dz 0) (dw 0))
  `(%dquat ,(ensure-float-param x env)
           ,(ensure-float-param y env)
           ,(ensure-float-param z env)
           ,(ensure-float-param w env)
           ,(ensure-float-param dx env)
           ,(ensure-float-param dy env)
           ,(ensure-float-param dz env)
           ,(ensure-float-param dw env)))

(defmethod print-object ((d dquat) stream)
  (write (make-load-form d) :stream stream))

(defmethod make-load-form ((d dquat) &optional env)
  (declare (ignore env))
  `(dquat ,(qx d) ,(qy d) ,(qz d) ,(qw d)
          ,(dx d) ,(dy d) ,(dz d) ,(dw d)))
