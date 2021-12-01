FUNCTION FGCOMPOSITE::init, names, _REF_EXTRA=ex

	nmodels = N_ELEMENTS(names)

	ok = self->idl_container::init()
	ok = self->fgeom_frame::init( _EXTRA=ex )

	IF nmodels EQ 0 THEN $
		RETURN, ok
	
	self->fgeomfactory, names, _EXTRA=ex
	
	RETURN, ok
	
END

PRO FGCOMPOSITE::Add, object
	
	CASE N_ELEMENTS(object) OF
		0:RETURN
		1: $
		BEGIN
			object->setparent,self
			object->UseIterator,self.iterator
			self->IDL_Container::Add, object
		END
		ELSE: $
		BEGIN
			FOR i=0,N_ELEMENTS(object)-1 DO BEGIN
				object[i]->UseIterator,self.iterator
				self->Add,object[i]
			ENDFOR
		END
	ENDCASE

END

FUNCTION FGCOMPOSITE::intersect_test, sources, _REF_EXTRA=ex

	models = self->get(/ALL,COUNT=count)
	
	if count EQ 0 THEN return,-1
	
	FOR i=0,count-1 DO BEGIN
		;copy = sources
		newblocked = models[i]->intersect_test(sources, _EXTRA=ex)
		
		constraint = models[i]->constraint()
		
		IF N_ELEMENTS(blocked) EQ 0 THEN blocked = newblocked $
		ELSE BEGIN 
			case (constraint AND 127B) of
				0B:blocked = blocked OR newblocked
				1B:blocked = blocked AND newblocked
				ELSE: blocked = blocked OR newblocked
			endcase
		END
	ENDFOR
	;print,self.constraint AND 128B
	if (self.constraint AND 128B) gt 1 then begin
		blocked = NOT (blocked)
	endif
	
	RETURN,blocked
END

PRO FGCOMPOSITE::fgeomfactory, names, _REF_EXTRA=ex

	nmodels = N_ELEMENTS(names)
	IF nmodels EQ 0 THEN RETURN
	
	FOR i=0,nmodels-1 DO BEGIN
		obj = OBJ_NEW(names[i])
		IF OBJ_VALID(obj) THEN $
			IF OBJ_ISA(obj,'FGEOM_FRAME') OR OBJ_ISA(obj,'FGCOMPOSITE') THEN self->add, obj ELSE OBJ_DESTROY,obj
	ENDFOR

END

PRO FGCOMPOSITE::IterateCallPro, method, params, _REF_EXTRA=ex

	objects = self->get(/ALL, _EXTRA=['ISA'], COUNT=count)
	
	if count EQ 0 THEN return
	
	FOR i=0,count-1 DO BEGIN
		IF OBJ_ISA(objects[i],'FGCOMPOSITE') THEN $
			CALL_METHOD,'IterateCallPro', objects[i],method, params, _EXTRA=ex  $
		ELSE IF OBJ_HASMETHOD(objects[i], method) THEN $
			CALL_METHOD,method, objects[i], params, _EXTRA=ex $
		ELSE print, 'Method '+method+' not found for object ',i
	ENDFOR

END

FUNCTION FGCOMPOSITE::IterateCallFunc, method, COUNT=count, PTR=ptr, _REF_EXTRA=ex

	objects = self->get(/ALL, _EXTRA=['ISA'], COUNT=count)
	
	if count EQ 0 THEN return,-1
	
	IF NOT OBJ_HASMETHOD(objects[0], method) THEN BEGIN
		print, "FGCOMPOSITE::IterateCallFunc Error: First object must have method '",method,"'"
		print, "First object is of class ", OBJ_CLASS(objects[0])
		RETURN,-1
	ENDIF
	
	returnType = CALL_METHOD(method, objects[0], _EXTRA=ex)
	IF KEYWORD_SET(ptr) THEN returns = [PTR_NEW(returnType,/no_copy)] ELSE $
	returns = [returnType]
	
	FOR i=1,count-1 DO BEGIN
		IF OBJ_HASMETHOD(objects[i], method) THEN BEGIN
			result = CALL_METHOD(method, objects[i], params, _EXTRA=ex)
			IF KEYWORD_SET(ptr) THEN returns = [returns, PTR_NEW(result,/no_copy)] $
			ELSE returns = [returns, result]
		ENDIF
	ENDFOR
	
	RETURN, returns

END

PRO FGCOMPOSITE::UseIterator,it
	self.iterator = it
END

PRO FGCOMPOSITE::setparent,object,_REF_EXTRA=ex & self.parent = object & END
FUNCTION FGCOMPOSITE::getparent,dummy,_REF_EXTRA=ex & RETURN,self.parent & END

PRO FGCOMPOSITE::Cleanup
	
	IF OBJ_VALID(self.parent) THEN $
		IF OBJ_HASMETHOD(self.parent, 'remove') THEN self.parent->remove, self
	
	self->IDL_Container::Cleanup
	
	OBJ_DESTROY,self.iterator

END

PRO fgcomposite__define

	struct = {FGCOMPOSITE,$
			INHERITS FGEOM_FRAME,$
			INHERITS IDL_CONTAINER,$
			iterator:OBJ_NEW() }

END