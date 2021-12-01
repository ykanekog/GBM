FUNCTION solar_array_minus::init

	self.name = "-SCA"

	status = self->solar_arrays::init()
	rpos = -96.3
	inward = -45;-91.2
	z_mount = 41.5 ;+ 1.1
	
	self.pos =	[0.0, rpos + inward, z_mount]
	
	self->reset
	
	self.axis = [0.0, -1.0, 0.0]
	
	self.o_quat = OBJ_NEW('FGEOM_QUATERNION', ANGLE=0.0,$
							RAXIS=self.axis )
	
	RETURN,status	
END

PRO solar_array_minus::reset

	inward = self.pos[1] + 96.3
	
	z_mount = 41.5
	
	hw = self.width / 2.0 ;+ 10
	
	sca_mi_verts = $
			[ 	$
				[0.0, 0, z_mount + hw], $
				[0.0, 0 - self.length, z_mount + hw], $
				[0.0, 0 - self.length, z_mount - hw], $
				[0.0, 0, z_mount - hw ], $
				[0.0, 0, z_mount + hw] $
			]
			
	;sca_mi_verts[0,*] += 5
	
	sca_mi_verts[1,*] += self.pos[1]
	sca_mi_verts[1,1:2] += (-91.2 - inward)
	
	;print,'neg', sca_mi_verts[1,1:2]
	
	self->set_verts, sca_mi_verts
	self.angle = 0.0
	
END

PRO solar_array_minus__define

	struct = {solar_array_minus, $
		INHERITS SOLAR_ARRAYS $
	}

END