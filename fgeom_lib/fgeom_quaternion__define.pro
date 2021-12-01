;---------------------------------------------------------------------------
;
;	FILE: fgeom_quaternion__define.pro
;
;	CLASS:  fgeom_quaternion
;
;	BY: Vandiver L. Chaplin, The University of Alabama in Huntsville
;
;	VERSION: 2.0 - January 20,2010
;
;	DESCRIPTION:
;	Implements a quaternion which can be used for 3-D rotations.
;	The quaternion can be defined using the 'set' method. Either
;	specify the 4 elements directly, or specify an axis of rotation as an XYZ 3-vector
;	and the counter-clockwise angle of rotation about that axis. Notice
;	the direction of the axis,angle rotation scheme is LEFT HANDED. 
;	To perform a right-handed rotation, negate either the angle or axis argument,
;	or transpose the matrix returned by rotator().
;
;	To create and define the quaternion, do one of the following:
;
;	1.  Q = 4-element array = [qs, {qx, qy, qz} ] ( curly braces indicate the complex 3-vector ).
;	2.  W = qs, V = [qx,qy,qz]
;	3.	RAXIS = [x,y,z], ANGLE = ccw_angle
;	These are keywords to the 'set' method, but can be used in the constructor,too:
;
;	i) in one step:
;		q = OBJ_NEW('fgeom_quaternion', RAXIS = [x,y,z], ANGLE = ccw_angle)
;	ii) in two steps:
;		q = OBJ_NEW('fgeom_quaternion')
;		q->set, RAXIS = [x,y,z], ANGLE = ccw_angle
;
;
;	Because of the use of trig functions, the quaternion needs to be normalized
;	periodically to ensure it is a unit quaternion if its values are being changed.
;	The public routine 'mkunit' ( e.g., q->mkunit ) does this.
;
;
;	ROTATIONS:
;	Quaternion rotations on 3-vectors can be expressed as matrix multiplication, vp = A*v.
;	v is the original vector and vp the rotated vector.  The matrix A is returned by the
;	method rotator().
;	Using this class, rotations on cartesian vectors can be performed in several ways.
;
;	IDL's '#' operator allows v to be a 3 x N array, so to rotate a set of N vectors, say
;	vertices of a 3-D structure, do one of the following:
;
;	i) Return the matrix and multiply the vectors directly:
;		A = q->rotator()
;		vp = A # v  	;or transpose(A) for the opposite rotation
;	ii) Rotate and replace the data in v.  v can be a 3 x N array or a pointer
;		to a 3 x N array:
;		boolean = q->rotate(v)  (returns succes or failure)
;	
;	Additionally, an OFFSET=[x,y,z] can be passed to rotate() to ensure the 
;	model is rotated 'in place' (i.e., the model frame). If OFFSET is supplied,
;	it is subtracted from each vector prior to rotation, and the added back after.
;	In other words, the transformation from model coordinates to 'world' coordinates
;	(i.e., v is in world coordinates) is a simple translation.  OFFSET is essentially
;	the base of the rotation axis.
;
;---------------------------------------------------------------------------



FUNCTION quat_multiply, q, p
	q3 = q[1:3]
	p3 = p[1:3]
	
	pq_s = p[0]*q[0] - total(p3*q3)
	pq_3 = p[0]*q3 + q[0]*p3 + crossp(p3,q3)

	RETURN, [pq_s, pq_3]
END

FUNCTION wxyz_to_quaternion, wxyz
	IF N_ELEMENTS(wxyz) NE 4 THEN RETURN, 0
	RETURN, [ cos(wxyz[0] * !DTOR / 2) , wxyz[1:3] * sin(wxyz[0] * !DTOR / 2)]
END

FUNCTION fgeom_quaternion::init, _REF_EXTRA = ex
	self.q = [1.0, 0.0, 0.0, 0.0]
	self->set, _EXTRA = ex
	self->mkunit
	RETURN,1
END

PRO fgeom_quaternion::update_wxyz, p
	p = wxyz_to_quaternion(p)	
	self.q = quat_multiply( self.q, p )
END

; The rotator matrix here has a 'left-handed' rotation w.r.t to the axis, angle
	; used in wxyz_to_quaternion. That is, ANGLE is the counter-clockwise rotation
	; about RAXIS.  This is so GLAST quaternions can be used as written in the data,
	; to go from ECI to SC coordinates, which is the transpose of the right-handed rotation.

PRO fgeom_quaternion::set, ANGLE=angle, RAXIS=raxis, W=w, V=v, Q=q
	
	IF N_ELEMENTS(raxis) EQ 3 AND N_ELEMENTS(angle) EQ 1 THEN BEGIN
		raxis = raxis / norm(raxis)
		q = wxyz_to_quaternion( [angle, raxis] )	
	ENDIF	
	IF N_ELEMENTS(q) EQ 4 THEN BEGIN
		self.q = q
		RETURN
	ENDIF

	IF N_ELEMENTS(w) EQ 1 AND N_ELEMENTS(v) EQ 3 THEN BEGIN
		self.q[0] = w
		self.q[1:3] = v
	ENDIF
	
	;self.q = self.q / norm(self.q)
END

PRO fgeom_quaternion::mkunit
	self.q = self.q / norm(self.q)
END

PRO fgeom_quaternion::printq
	print, self.q
END


FUNCTION fgeom_quaternion::rotate, v_ptr, OFFSET=vector
	
	isheapvar = n_elements(v_ptr) eq 1
	
	IF isheapvar && OBJ_VALID(v_ptr) && OBJ_ISA(v_ptr, 'FGEOM_FRAME') THEN BEGIN
		verts = v_ptr->getverts(FROM=vector)
		rotation = self->rotator() # verts
		v_ptr->set_verts, rotation, TO=vector
		RETURN, 1
	ENDIF
	
	IF N_ELEMENTS(vector) EQ 3 THEN BEGIN
	
		IF isheapvar && PTR_VALID(v_ptr) THEN BEGIN
			FOR i=0,2 DO (*v_ptr)[i,*] -= vector
			*v_ptr = self->rotator() # *v_ptr
			FOR i=0,2 DO (*v_ptr)[i,*] += vector
			RETURN, 1
		ENDIF
		
		FOR i=0,2 DO v_ptr[i,*] -= vector
		v_ptr = self->rotator() # v_ptr
		FOR i=0,2 DO v_ptr[i,*] += vector
		RETURN, 1
		
	ENDIF 
	
	IF isheapvar && PTR_VALID(v_ptr) THEN BEGIN
		*v_ptr = self->rotator() # *v_ptr
		RETURN,1
	ENDIF ELSE BEGIN
		v_ptr = self->rotator() # v_ptr
		RETURN,1
	ENDELSE
	
	RETURN,0
	
END

FUNCTION fgeom_quaternion::getq
	RETURN, self.q
END

; Mutator is just the 3-D rotation matrix with an additional 
; column describing a translation.  it is a 4x4 matrix, with the last
; row M[*,3] = [0,0,0,1], and the last column M[3,*] = [dx,dy,dz,1],
; where {dx,dy,dz} is a translation to applied after rotation.
; This for use in the common 4-vector 
; sheme, vp = M # v, where v = {vx,vy,vz,0}, and M is this mutator. vp
; is the original vector after being rotated and then translated.
;
; This function returns a 4x4 matrix corresponding to the rotation
; and zero translation.  Set M[3,*] = [dx,dy,dz,1] for the desired
; translation.

FUNCTION fgeom_quaternion::mutator

	mutator = FLTARR(4,4)
	
	mutator[0:2,0:2] = self->rotator()

	RETURN,matrix
END

FUNCTION fgeom_quaternion::rotator

	matrix = FLTARR(3,3)
	q = self.q
	
	s = 0b
	x = 1b
	y = 2b
	z = 3b
	
	; The rotator matrix here has a 'left-handed' rotation w.r.t to the axis, angle
	; used in wxyz_to_quaternion. That is, ANGLE is the counter-clockwise rotation
	; about RAXIS.  This is so GLAST quaternions can be used as described by GD,
	; to go from ECI to SC coordinates.  If the matrix below were transposed,
	; the AXIS and ANGLE concepts implemented in this class would be
	; right-handed (i.e., clockwise rotation by ANGLE when 'looking' down AXIS).

	matrix[0,0] =  q[x]^2 - q[y]^2 - q[z]^2 + q[s]^2
	matrix[1,1] = -q[x]^2 + q[y]^2 - q[z]^2 + q[s]^2
	matrix[2,2] = -q[x]^2 - q[y]^2 + q[z]^2 + q[s]^2
	
	matrix[0,1] = 2 *( q[x]*q[y] + q[z]*q[s] )
	matrix[0,2] = 2 *( q[x]*q[z] - q[y]*q[s] )
	
	matrix[1,0] = 2 *( q[x]*q[y] - q[z]*q[s] )
	matrix[1,2] = 2 *( q[x]*q[s] + q[y]*q[z] )
	
	matrix[2,0] = 2 *( q[x]*q[z] + q[y]*q[s] )
	matrix[2,1] = 2 *( q[y]*q[z] - q[x]*q[s] )
	

	RETURN,matrix
END

PRO fgeom_quaternion::Cleanup
	OBJ_DESTROY,self
END

PRO fgeom_quaternion__define
	
	struct = {FGEOM_QUATERNION, $
			q:FLTARR(4) $
			}

END