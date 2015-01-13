all: first simple if arith rec

.PHONY: first simple if arith rec clean


simple:
	(cd simple ; make)

first:
	(cd first  ; make)

if:
	(cd if     ; make)

arith:
	(cd arith  ; make)

rec:
	(cd rec    ; make)

clean:
	(cd simple ; make clean)
	(cd first  ; make clean)
	(cd if     ; make clean)
	(cd arith  ; make clean)
	(cd rec    ; make clean)

