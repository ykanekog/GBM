FUNCTION fgeneric_iterator::init
	return,1
END

FUNCTION fgeneric_iterator::pp_inc
	self.iterand++
	return, self.iterand
END

FUNCTION fgeneric_iterator::inc_pp
	self.iterand++
	return, self.iterand-1
END

FUNCTION fgeneric_iterator::pp_dec
	self.iterand--
	return, self.iterand
END

FUNCTION fgeneric_iterator::dec_pp
	self.iterand--
	return, self.iterand+1
END

FUNCTION fgeneric_iterator::value
	return, self.iterand
END

PRO fgeneric_iterator__define

	struct = {FGENERIC_ITERATOR,$
				iterand:0L $
			}

END