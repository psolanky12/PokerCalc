.PHONY: test check

default:
			dune exec test/main.exe

clean:	
			dune clean

code:
	-dune build
	code .
	! dune build --watch

build:
		dune build
		rm -f pokergenerator.zip

test:
		OCAMLRUNPARAM=b dune exec test/main.exe

utop:
		OCAMLRUNPARAM=b dune utop lib

chat:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f poker.zip
	zip -r poker.zip . -x@exclude.lst

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh