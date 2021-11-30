FUNCTION sod_to_utc, sod, ISO=iso
	
	s = sod
;	print,s
	h = floor(s / 3600d); mod 24
	s -= h * 3600d
;	print,s
	m = floor(s / 60d)
	s -= m * 60d
;	print,s

	IF KEYWORD_SET(iso) THEN BEGIN
		ut = sod_to_utc(sod)
		hh = string(h, FORMAT='(I2)')
		mm = string(m, FORMAT='(I2)')
		ss = string(s, FORMAT='(F0.2)')
		
		IF h lt 10 THEN STRPUT, hh, '0',0
		IF m lt 10 THEN STRPUT, mm, '0',0
		IF s lt 10 THEN ss = '0' + ss
		
		RETURN, hh+':'+mm+':'+ss
	END ELSE RETURN, [h,m,s]
END

FUNCTION met_to_utc, met, _REF_EXTRA=ex

	if (size(met,/type) ne 5) then begin
		print, "Warning: MET parameter should be type DOUBLE to avoid losing precision"
		met = double(met)
	endif
	leapseconds = 0.0d
	if met gt 253497600. then leapseconds -= 2.0d
	
	mett1 = (met + leapseconds)
;	print,mett1
	days = floor(mett1 / 86400d)
	sod = mett1 - days * 86400d

	
	sod -= leapseconds
	RETURN, sod_to_utc(sod, _EXTRA=ex)
END