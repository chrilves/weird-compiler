all: wc.native

.PHONY: wc.native wc.byte owc.byte owc clean

wc.native:
	ocamlbuild wc.native
	mv _build/wc.native ..

wc.byte:
	ocamlbuild wc.byte
	mv _build/wc.byte ..

owc.byte: ../obrowser
	CAMLLIB=`pwd`/../obrowser/rt/caml ocamlbuild -ocamlyacc menhir owc.byte

owc.uue: owc.byte
	uuencode _build/owc.byte stdout > ../owc.uue
	cp -fv ../owc.uue ../../../tuxfamily/weird-compiler.tuxfamily.org-web/htdocs/owc
	


clean:
	/bin/rm -frv _build/ parser.ml parser.mli lexer.ml lexer.mli *~ *.cm*


../obrowser:
	cd .. ; make obrowser
