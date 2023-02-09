potatop/LispLibrary.h: lisp/LispLibrary
	(cd lisp; xxd -i LispLibrary) > potatop/LispLibrary.h

lisp/LispLibrary: lisp/*.lisp
	cat lisp/*.lisp > lisp/LispLibrary

.PHONY: clean

clean:
	rm -f potatop/LispLibrary.h lisp/LispLibrary
