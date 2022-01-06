#|
 This file is a part of 3d-quaternions
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.flare.quaternion)

;; ops.lisp
(docs:define-docs
  (function qsetf
    "Update the fields of a quaternion.

Returns the modified quaternion.

See QUAT")
  
  (function qfrom-angle
    "Construct a quaternion from an axis and a rotation.

Returns the freshly constructed quaternion.

See QUAT")
  
  (function qtowards
    "Construct a quaternion that describes the rotation from one vector to another.

Returns the freshly constructed quaternion.

See QUAT")
  
  (function qaxis
    "Returns the normalised axis around which the quaternion rotates.

This is the same as just calling VUNIT.

See QUAT
See 3D-VECTORS:VUNIT")
  
  (function qangle
    "Returns the angle around the rotation axis that the quaternion rotates by.

See QUAT")
  
  (function q=
    "Returns true if the quaternions passed are the same.

This does not test for element-wise float exact equality,
and instead compares accounting for a minimal epsilon of difference.

See QUAT")
  
  (function q/=
    "Returns true if any of the quaternions passed are not the same.

This does not test for element-wise float exact equality,
and instead compares accounting for a minimal epsilon of difference.

See QUAT")
  
  (function qequal
    "Returns true if the quaternions passed describe the same direction.

This disregards the orientation of the rotation encoded, and treats
them the same regardless of whether they rotate around the 'long way'
or the 'short way'.

See QUAT
See Q=")
  
  (function q+
    "Returns the element-wise addition of the passed quaternions.

You may also pass a real number to add to each component.

See QUAT
See NQ+")
  
  (function nq+
    "Returns the first quaternion after being modified by element-wise addition of all passed quaternions.

You may also pass a real number to add to each component.

See QUAT
See Q+")
  
  (function q-
    "Returns the element-wise subtraction of the passed quaternions.

You may also pass a real number to subtract from each component.

See QUAT
See NQ+")
  
  (function nq-
    "Returns the first quaternion after being modified by element-wise subtraction of all passed quaternions.

You may also pass a real number to subtract from each component.

See QUAT
See Q-")
  
  (function q*
    "Returns the multiplication of the passed quaternions.

You may also pass a real number to multiply with each component.
Note that for quaternions this is *not* element-wise multiplication.

See QUAT
See NQ*")
  
  (function nq*
    "Returns the first quaternion after being modified by multiplication of the passed quaternions.

You may also pass a real number to multiply with each component.
Note that for quaternions this is *not* element-wise multiplication.

See QUAT
See NQ*")
  
  (function q/
    "")
  
  (function nq/
    "")
  
  (function q.
    "Returns the dot product of the two quaternions.

See QUAT")
  
  (function qlength2
    "Returns the squared length of the quaternion.

See QUAT
See QLENGTH")
  
  (function qlength
    "Returns the length of the quaternion.

See QUAT
See QLENGTH2")
  
  (function qunit
    "Returns a normalised copy of the quaternion.

See QUAT
See NQUNIT")
  
  (function nqunit
    "Returns the quaternion after normalising it.

See QUAT
See QUNIT")
  
  (function qconjugate
    "Returns the conjugate of the quaternion.

See QUAT")
  
  (function qinv
    "Returns the inverses of the quaternion.

See QUAT")
  
  (function q*v
    "Returns the vector after multiplying it by the quaternion.

This essentially rotates the vector by the rotation expressed by the
quaternion.

See QUAT
See 3D-VECTORS:VEC3")
  
  (function qmix
    "Returns a new quaternion mixed with the two.

This is essentially Q = A*(1-X) + B*X

See QUAT")
  
  (function qnlerp
    "Returns the linearly interpolated quaternion between the two.

This is essentially Q = A + (B-A)*X

See QUAT")
  
  (function qexpt
    "Compute the exponentiation of a quaternion.

Returns a fresh quaternion.

See QUAT")
  
  (function nqexpt
    "Compute the exponentiation of a quaternion.

Returns the modified quaternion.

See QUAT")
  
  (function qslerp
    "Returns the spherically interpolated quaternion between the two.

This attempts to compute the interpolation by rotating along the unit
sphere.

See QUAT")
  
  (function qlookat
    "Returns a quaternion that encompassses the rotation necessary to look in the described direction.

See QUAT")
  
  (function qmat3
    "Returns a MAT3 that encompasses the rotation described by the quaternion.

See QUAT
See QMAT4
See QFROM-MAT
See 3D-MATRICES:MAT3")
  
  (function qmat4
    "Returns a MAT4 that encompasses the rotation described by the quaternion.

See QUAT
See QMAT3
See QFROM-MAT
See 3D-MATRICES:MAT4")
  
  (function qfrom-mat
    "Returns a quaternion that encompasses the same rotation described by the matrix.

Both a MAT3 and MAT4 can be used.

See QUAT
See QMAT3
See QMAT4
See 3D-MATRICES:MAT3
See 3D-MATRICES:MAT4"))

;; struct.lisp
(docs:define-docs
  (type quat
    "Encompasses a quaternion.

A quaternion describes a rotational axis and an angle.
This is a sub-structure of a VEC3, and as such can be used with any
functions from 3D-VECTORS that support VEC3s.

See 3D-VECTORS:VEC3
See QUAT (function)
See QCOPY
See QUAT-P
See QX
See QY
See QZ
See QW")
  
  (function quat
    "Constructs a quaternion.

If no arguments are passed, a unit quaternion of 0 0 0 1 is returned.

See QUAT (type)")
  
  (function qcopy
    "Creates an exact copy of the quaternion.

See QUAT")
  
  (function quat-p
    "Returns true if the passed object is a quaternion.

See QUAT")
  
  (function qx
    "Accesses the first component of the quaternion.

See QUAT (type)")
  
  (function qy
    "Accesses the second component of the quaternion.

See QUAT (type)")
  
  (function qz
    "Accesses the third component of the quaternion.

See QUAT (type)")
  
  (function qw
    "Accesses the fourth component of the quaternion.

See QUAT (type)")

  (type dquat
    "")
  
  (function dquat
    "")
  
  (function dqcopy
    "")
  
  (function dquat-p
    "")
  
  (function dx
    "")
  
  (function dy
    "")
  
  (function dz
    "")
  
  (function dw
    ""))
