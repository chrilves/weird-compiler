all: manual.pdf
	@echo 
	@echo /!\ IF YOU GET THE ERROR MESSAGE "contex.lua not found" or "base.lua not found"
	@echo ON ARCH LINUX, TYPE
	@echo
	@echo '  sudo sed -i "s|selfautoparent:|/usr/share/|g" "/usr/share/texmf/web2c/texmfcnf.lua"'
	@echo
	@echo THEN IT MIGHT WORK.
	@echo
	@echo Documentation is in doc/manual.pdf
	@echo

manual.pdf: manual.tex
	context manual.tex
	cp -v manual.pdf ..

clean:
	/bin/rm -frv texmf-var *tuc *pdf *log *~
