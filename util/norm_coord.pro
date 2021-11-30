FUNCTION norm_coord, r 
;   r = float(r)

	diff = r[1] - r[0]
	
	scale = [  -r[0] / diff, 1.0 / diff ]

	RETURN,scale
END 