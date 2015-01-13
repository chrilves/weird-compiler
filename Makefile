VERSION=0.2.2
DIR=$(shell basename `pwd`)
ARCHNAME=weird-compiler

ARCHTAR=${ARCHNAME}-${VERSION}.tar.bz2
ARCHZIP=${ARCHNAME}-${VERSION}.zip
DIRVER=${DIR}-${VERSION}

OBROWSERVER=1.1.1
OBROWSERDIR=obrowser-${OBROWSERVER}
OBROWSERFILE=${OBROWSERDIR}.tar.gz
OBROWSERURL=http://ocsigen.org/download/${OBROWSERFILE}


all: clean bin examples doc archive owc

bin: wc.native wc.byte owc.uue

wc.native:
	( cd src ; make wc.native)

wc.byte:
	( cd src ; make wc.byte)

owc.uue: obrowser/vm.js
	( cd src ; make owc.uue )

examples: wc.native
	(cd examples ; make)

doc:
	(cd doc  ; make)

.PHONY: clean bin edit examples package wc.native doc

clean:
	(cd src      ; make clean)
	(cd examples ; make clean)
	(cd doc      ; make clean)
	/bin/rm -frv dist/
	find . \(     -iname "*~"           \
	          -or -iname "*.output"     \
	          -or -iname "*.pdf"        \
	          -or -iname "*.log"        \
	          -or -iname "*.native"     \
	          -or -iname "*.byte"       \
	          -or -iname "*.uue"        \) -exec /bin/rm -frv {} \;

# package the sources
archive:
	/bin/rm -frv dist/
	mkdir -v -p dist/"${DIRVER}"
	git clone . dist/"${DIRVER}"
	cd dist/ ; \
          find "${DIRVER}" -name ".git" -type d -exec /bin/rm -frv {} \; ; \
          tar -jcf "${ARCHTAR}" "${DIRVER}" ; \
          zip -r   "${ARCHZIP}" "${DIRVER}"


owc: owc.uue obrowser/vm.js
	/bin/rm -frv dist/owc/
	mkdir -p dist/owc/ dist/owc/examples/
	cp -v owc.uue dist/owc/
	cp -rv owc/* tex/ shell/ dist/owc/
	find examples -iname "*.wc" -exec cp -v {} dist/owc/examples/ \;
	cp obrowser/vm.js dist/owc


# OBROWSER COMPILATION

obrowser: ${OBROWSERDIR}
	ln -s ${OBROWSERDIR} obrowser

obrowser/vm.js:	obrowser
	cd obrowser ; make

${OBROWSERFILE}:
	wget "${OBROWSERURL}"

${OBROWSERDIR}: ${OBROWSERFILE}
	tar -xf "${OBROWSERFILE}"

