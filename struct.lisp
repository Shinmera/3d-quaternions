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
(define-vecx-accessor qi %vx3)
(define-vecx-accessor qj %vy3)
(define-vecx-accessor qk %vz3)
(define-vecx-accessor qr %qw)

(declaim (inline quat))
(declaim (ftype (function (&optional real real real real) quat) quat))
(define-ofun quat (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
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
                  (:constructor %dquat (qreal qdual))
                  (:copier dqcopy)
                  (:predicate dquat-p))
  (qreal (quat) :type quat)
  (qdual (quat) :type quat))

(declaim (inline dquat))
(declaim (ftype (function (&optional quat quat) dquat) dquat))
(define-ofun dquat (&optional (real (quat)) (dual (quat 0 0 0 0)))
  (%dquat real dual))

(define-compiler-macro dquat (&environment env &optional (real '(quat)) (dual '(quat 0 0 0 0)))
  `(%dquat ,real ,dual))

(defmethod print-object ((d dquat) stream)
  (write (make-load-form d) :stream stream))

(defmethod make-load-form ((d dquat) &optional env)
  (declare (ignore env))
  `(dquat ,(qreal d) ,(qdual d)))
