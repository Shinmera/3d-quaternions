#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.quaternion)

(declaim (inline %quat))
(defstruct (quat (:conc-name NIL)
                 (:constructor %quat (%qx %qy %qz %qw))
                 (:copier qcopy)
                 (:predicate quat-p))
  (%qx (ensure-float 0) :type #.*float-type*)
  (%qy (ensure-float 0) :type #.*float-type*)
  (%qz (ensure-float 0) :type #.*float-type*)
  (%qw (ensure-float 0) :type #.*float-type*))

(define-vecx-accessor qx %qx)
(define-vecx-accessor qy %qy)
(define-vecx-accessor qz %qz)
(define-vecx-accessor qw %qw)

(declaim (inline quat))
(declaim (ftype (function (real real real real) quat) quat))
(define-ofun quat (x y z w)
  (%quat (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)))

(define-compiler-macro quat (&environment env x y z w)
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
                  (:inherit quat)
                  (:constructor dquat (%qx %qy %qz %qw %dx %dy %dz %dw))
                  (:copier dqcopy)
                  (:predicate dquat-p))
  (%dx (ensure-float 0) :type #.*float-type*)
  (%dy (ensure-float 0) :type #.*float-type*)
  (%dz (ensure-float 0) :type #.*float-type*)
  (%dw (ensure-float 0) :type #.*float-type*))

(define-vecx-accessor dx %dx)
(define-vecx-accessor dy %dy)
(define-vecx-accessor dz %dz)
(define-vecx-accessor dw %dw)

(declaim (inline dquat))
(declaim (ftype (function (real real real real real real real real) dquat) dquat))
(define-ofun dquat (x y z w dx dy dz dw)
  (%dquat (ensure-float x) (ensure-float y) (ensure-float z) (ensure-float w)
          (ensure-float dx) (ensure-float dy) (ensure-float dz) (ensure-float dw)))

(define-compiler-macro dquat (&environment env x y z w dx dy dz dw)
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
  `(dquat ,(qx q) ,(qy q) ,(qz q) ,(qw q)
          ,(dx q) ,(dy q) ,(dz q) ,(dw q)))
