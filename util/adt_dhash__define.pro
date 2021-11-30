;   Class: ADT_DHASH
;
;	AUTHOR: V.Chaplin, UAH, July 2009
;
;	Mapping from string keys to arbitrary data type values.
;	The data are copied into IDL pointers. Pointer allocation/freeing
;	is handled internally.  Because the data is stored in pointers,
;	it's more efficient to use a specific type hash if there are lots of reads and writes.
;	An example would be string-to-int hash, where the data table is just and integer array,
;	rather than an array of pointers.  This could be achieved by subclassing (discussed below).
;
;	KEY ADDRESSING:
;	This object uses a base36 initial hash function, and a double
;	hash function dhash_inr() to resolve collisions.  The table size
;	is fixed and set at construction.
;
;	The method collision(key, addr) defines how to check for a collision.
;	By default it uses the field *self.undef, which defines what an undefined
;	key looks like. If the key at addr equals *self.undef, it returns false (no collision).
;	If the key at addr equals the lookup key, it assumed to not 
;	be a collision, it returns false, and the value at addr will be overwritten. Otherwise
;	it returns true.
;
;	For specializing hashing methods there are several options available.
;	The easiest would be to re-define lookup(), which is just a wrapper for the primary hash
;	function, base36() in this case.  dhash_incr() defines the double-hash increment value.
;	The function hashf() calls lookup(), collision(), and dhash_incr(), but could be overwritten
;	directly.  In this class it ultimately implements the double hashing/open addressing method.
;	Finally, the table access routines, discussed under INHERITANCE below, call hashf(), so these
;	could be written to call a different function for a completely different kind of hashing.
;
;	DATA STORAGE:
;	This class is designed to be a base clase for more specific hash types,
;	but it can be used on it's own for string => any IDL type mapping.
;	It uses the most generic data storage available in IDL, a pointer
;	to an array of pointers, each of which points to a mapped value. This makes
;	it completely generic, but inefficient and slow for large table, or lot's of read/writes.
;
;	The only way to increase the hash size is to create a new hash and use the method
;	CopyDataInto( new_hash ) to copy data from self to new_hash.  This
;	routine uses self->get() and new_hash->set, meaning it creates a completely
;	distinct memory allocation in the heap, so self can be destroyed at this point
;	(but see the case where the hash contains heap variables, discussed below).
;	This makes it very inefficient for lots of dynamic resizing.
;
;	Key values are stored in PTR heap variables.  Thus
;	mapping to a heap variable, such as an object, results
;	in a PTR_NEW( OBJ_NEW() ) type of construction (actually the object
;	reference is just copied into the pointer, not reconstructed, so it
;	mantains its state).  The default OBJ_DESTROY for this object is to
;   perform HEAP_FREE on all data values, which will recursively free all heap memory
;	referenced by this object.  The optional /HANDLE_ONLY to the 
;	destructor deletes only the private references in the object, so it does not 
;	delete any pointed-to heap variables.  /HANDLE_ONLY could result in 
;	dangling pointers without careful attention.
;
;	SUB-CLASSING and INHERITANCE:
;	
;	Creating sub-class for specific data types is more efficient because it could mean
;	allocating the data table directly as an array.  For full tables, this would be the best
;	use of memory and the fastest access option.  For large, sparse tables
;	this might mean inefficient memory usage, though access would be faster.
;
;	All sub-classes that store data differently must re-define:
;	Constructor (aka, ::init) - defines key type, value type, and table size.  Allocates key & value arrays.
;	AccessData( )
;	WriteToTable
;	DeleteFromTable
;
;	The last three methods all should be considered 'virtual' and 'protected' in typical OO terminology,
;   though IDL doesn't provide this specific facility.
;	They shouldn't be called directly by clients, but via the common interface routines get, set, delete, etc.
;	They all take an integer argument 'address', which is returned by the 
;	function hashf(key).  In open addressing, hashf() first calls lookup(),
;	which returns an address, then checks for a collision with collision().
;	If true, the address is incremented until the collision is resolved. The value
;	of this increment is returned by the double-hash function dhash_incr().
;
;	Sub-classes using string keys must only re-define the constructor
;	(to allocate the data table), and the routines AccessData(), WriteToTable, 
;	and DeleteFromTable.

;	If the table uses a different key type than strings, such as integers,
;	then the routine lookup() must also be re-defined to accept an integer argument
;	and call an appropriate hashing function, such as division or radix.  The
;	constructor must also set self.keys to PTR_NEW( intarr( size ) ).
;
;------------------------------------------------------------------------------------------

FUNCTION adt_dhash::init, tbl_size,emptyp, LOADFACTOR=load

	IF N_ELEMENTS(tbl_size) EQ 0 THEN self.size = 100L $
	ELSE self.size = tbl_size
	
	IF N_ELEMENTS(load) EQ 0 THEN load = .55
	
	self.maxLoad = LONG(load * self.size)
	
	table = PTRARR(self.size)
	self.handle = PTR_NEW(table)
	
	self.keys = PTR_NEW(STRARR(self.size))
	
	self.undef = PTR_NEW('')
	
	RETURN,1

END

;Generic - Can be super classed
PRO adt_dhash::SetUndef,value

	conflict = WHERE( (*self.keys EQ value) AND (*self.keys NE *self.undef), count)
	
	IF count GT 0 THEN BEGIN
		print,"Warning, new undefined representation conflicts with existing keys."+$
				"Aborted. Copy, delete, and rename those keys to use this value."
		RETURN
	END

	currentundef = WHERE(*self.keys EQ *self.undef, count)
	IF count GT 0 then (*self.keys)[currentundef] = value

	PTR_FREE,self.undef

	self.undef = PTR_NEW(value)
END

;Generic - Can be super classed
FUNCTION adt_dhash::AccessKey,key,IFEXISTS=ifexists

	If n_elements(key) eq 0 then return, -1

	IF size(key,/type) eq 7 && key eq '' THEN BEGIN
		print, "ERROR: Null string cannot be used as a hash key"
		RETURN,-1
	ENDIF
	
	IF key eq *self.undef THEN BEGIN
		print, "ERROR: This key is currently used to represent undefined values. Use '->SetUndef,key' to change."
		RETURN,-1
	ENDIF
	
	i = self->hashf(key)
	IF keyword_set(ifexists) THEN IF (i eq -1) || ((*self.keys)[i] eq *self.undef) then return, -1
	;else return, i
	 return, i
END

;Must be subclassed for table handle structure
FUNCTION adt_dhash::AccessData,address
	IF address EQ -1 THEN RETURN, PTR_NEW()
	IF NOT PTR_VALID((*self.handle)[address]) THEN RETURN,PTR_NEW() $
	ELSE RETURN,  *((*self.handle)[address])[0]
END

;Must be subclassed for table handle structure
PRO adt_dhash::WriteToTable,address,value
	IF PTR_VALID((*self.handle)[address]) THEN BEGIN
		*((*self.handle)[address])[0] = value
		RETURN
	ENDIF
	(*self.handle)[address] = PTR_NEW(value)
END
;Must be subclassed for table handle structure
PRO adt_dhash::DeleteFromTable,address,GC=gc
	IF NOT PTR_VALID((*self.handle)[address]) THEN RETURN
	IF KEYWORD_SET(gc) then HEAP_FREE, (*self.handle)[address] $
	ELSE PTR_FREE, (*self.handle)[address]
END

;Generic - Can be super classed if WriteToTable is virtualized
PRO adt_dhash::set,key,value,I=i,_REF_EXTRA=ex

	ON_ERROR,2
	
	i = self->AccessKey(key)	
	IF i EQ -1 THEN RETURN
	
	self->WriteToTable,i,value
	(*self.keys)[i] = key
END
;Generic - Can be super classed if DeleteFromTable is virtualized
PRO adt_dhash::delete,key, ALL=all, _REF_EXTRA=ex

	ON_ERROR,2
	
	if keyword_set(all) then begin	
		keys = self->keys(nkeys)
		if nkeys eq 0 then return

		for n=0,nkeys-1 do begin
			i = self->AccessKey(keys[n],/IFEXISTS)
			self->DeleteFromTable,i,_EXTRA=ex
			(*self.keys)[i] = *self.undef
		endfor
		
		return
	endif
	
	i = self->AccessKey(key,/IFEXISTS)
	IF i EQ -1 THEN RETURN	
	
	self->DeleteFromTable,i,_EXTRA=ex
	(*self.keys)[i] = *self.undef
END
;Generic - Can be super classed
PRO adt_dhash::remove,key,_REF_EXTRA=ex
	self->delete,key,_EXTRA=ex
END
;Generic - Can be super classed.  Change ISA to superclass
FUNCTION adt_dhash::CopyDataInto, another_hash
	IF ~( OBJ_VALID(another_hash) && OBJ_ISA(another_hash,'ADT_DHASH') ) THEN RETURN,0
	IF another_hash eq self THEN RETURN,0
	
	keys = self->keys(count)
	IF count EQ 0 THEN RETURN,-1
	
	FOR i=0,count-1 DO BEGIN
		thisvalue = self->get(keys[i])
		another_hash->set,keys[i],thisvalue,I=placement
		IF placement EQ -1 THEN print, "Warning, uncopied key:",keys[i]
	ENDFOR

	RETURN,1
END

;Generic - Can be super classed.
PRO adt_dhash::print
	keys = self->keys(count)
	IF count EQ 0 THEN BEGIN
		print,self,'  EMPTY HASH'
		RETURN
	ENDIF
	
	FOR i=0,count-1 DO BEGIN
		thisvalue = self->get(keys[i])
		print, keys[i], thisvalue
	ENDFOR
END
;Generic - Can be super classed.

FUNCTION adt_dhash::exists,key
	i = self->AccessKey(key, /IFEXISTS)
	
	if ( i eq -1) then return, 0
	return, 1
END

FUNCTION adt_dhash::get,key
	i = self->AccessKey(key, /IFEXISTS)	

	;IF ((*self.keys)[i] NE key) THEN RETURN, PTR_NEW()
	;print,'get ', i,', got ',self->AccessData(i)
	RETURN, self->AccessData(i)	
END

FUNCTION adt_dhash::keys, count
	inds = WHERE(*self.keys NE *self.undef, count)
	IF count GT 0 THEN RETURN, (*self.keys)[inds] $ 
	ELSE RETURN,-1
END

FUNCTION adt_dhash::dhash_incr, key
	i = self->lookup(key)
	RETURN, (i^2 + (i-1)^2 ) MOD self.size
	;RETURN, 1
END

;Possible generic
;	By default it uses the field *self.undef, which defines what an undefined
;	key looks like. If the key at addr equals *self.undef, it returns false (no collision).
;	If the key at addr equals the lookup key, it assumed to not 
;	be a collision, it returns false, and the value at addr will be overwritten. Otherwise
;	it returns true.
FUNCTION adt_dhash::collision, key, address
	;RETURN,PTR_VALID((*self.handle)[address])) AND ((*self.keys)[address] NE key
	RETURN,( (*self.keys)[address] NE *self.undef ) && ( (*self.keys)[address] NE key )
END

;Generic - Can be super classed if collision() is generic or virtualized.
FUNCTION adt_dhash::hashf, key

	i = self->lookup(key)
	j = 0L
	WHILE ( self->collision(key, i) ) DO BEGIN
		;print,key, i
		i = (i + self->dhash_incr(key) ) MOD self.size
		IF j EQ self.maxLoad THEN BEGIN
			print,"ADT_DHASH::HASHF(): Unresolved collision with key ",key, " at ", i
			RETURN,-1
		ENDIF
		j++
	ENDWHILE

	RETURN,i
END

;Can be inherited, but key type must be appropriate for subclasses.
;
FUNCTION adt_dhash::lookup,key
	IF SIZE(key, /TYPE) NE 7 THEN key = STRTRIM(STRING(key),2)
	RETURN, self->base36(key)
END

FUNCTION adt_dhash::base36, key	
	index = ULONG(0)
	bkey = BYTE(key)

	FOR i=0L,strlen(key)-1 DO BEGIN
		index += ULONG( ( bkey[i] - BYTE('0') + 1 )* (36L^(strlen(key)- i - 1)) )
	ENDFOR
	index = index MOD self.size
	RETURN,index
END

FUNCTION adt_dhash::maxsize
	return, self.size
END

PRO adt_dhash::Cleanup,HANDLE_ONLY=handle_only
	IF KEYWORD_SET(handle_only) THEN BEGIN
		keys = self->keys(count)
		IF count GT 0 THEN FOR i=0,count-1 DO self->delete,keys[i]
		PTR_FREE,self.handle 
		
	ENDIF ELSE HEAP_FREE,self.handle
	
	PTR_FREE,self.keys
	PTR_FREE,self.undef
END

PRO adt_dhash__define

	struct = {ADT_DHASH, $
		size:0L,$
		maxLoad:0L,$
		handle:PTR_NEW(), $
		keys:PTR_NEW(), $
		undef:PTR_NEW() $
		}
	

END