EXAMPLE=simple

all: ${EXAMPLE}.pdf ${EXAMPLE}.sh

${EXAMPLE}.pdf: ${EXAMPLE}.tex
	pdftex ${EXAMPLE}.tex
	@echo -e "\nExample: ${EXAMPLE}:"
	@echo -e "  Source   file at examples/${EXAMPLE}/${EXAMPLE}.wc"
	@echo -e "  Compiled file at examples/${EXAMPLE}/${EXAMPLE}.tex"
	@echo -e "  PDF      file at examples/${EXAMPLE}/${EXAMPLE}.pdf"
	@echo -e "  TeX log  file at examples/${EXAMPLE}/${EXAMPLE}.log"


${EXAMPLE}.tex: ${EXAMPLE}.wc
	../../wc.native -t -i ${EXAMPLE}.wc -o ${EXAMPLE}.tex

${EXAMPLE}.sh: ${EXAMPLE}.wc
	../../wc.native -s -i ${EXAMPLE}.wc -o ${EXAMPLE}.sh
	chmod +x ${EXAMPLE}.sh

.PHONY: clean

clean:
	find . -type f -not \( -iname "*.wc" -or -iname "Makefile" \) -exec /bin/rm -fv {} \;
