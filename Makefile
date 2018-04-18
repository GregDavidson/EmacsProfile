# Emacs setup Makefile kas/jgd 4/18/18

HOME = /Users/Engineering/
EMACSHOME = $(HOME)/.emacs.d
INSTALLDIR = `pwd`
EMACS_SRC = *.el ../User-*/*.el

default: 
	@echo Use "make install" when ready
	@echo Or use 'make prepare' then link .emacs and init.el manually


install: prepare
	ln -s $(INSTALLDIR)/.emacs $(HOME)
	ln -s $(INSTALLDIR)/init.el $(EMACSHOME)
	ln -s $(INSTALLDIR)/README.org $(EMACSHOME)

prepare:
	mkdir -p $(EMACSHOME)		  # ensure EMACSHOME dir
	mkdir -p Limbo			  # ensure Limbo dir
	mv -if $(HOME)/.emacs Limbo	  # preserve your stuff
	mv -if $(EMACSHOME)/init.el Limbo # all your stuff...
	mv -if $(EMACSHOME)/README.org Limbo # all our stuff...

whereami:
	@echo $(INSTALLDIR)

TAGS: $(EMACS_SRC)
	etags -o $@ $^
