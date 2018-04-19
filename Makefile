# Emacs setup Makefile kas/jgd 4/18/18

HOME = ~
EMACSHOME = $(HOME)/.emacs.d
INSTALLDIR = `pwd`
EMACS_SRC = *.el ../User-*/*.el
USER_ME = $(EMACSHOME)/User-Me
INIT_ME = $(USER_ME)/init-me.el
OLD = $(HOME)/.emacs $(EMACSHOME)/init.el $(EMACSHOME)/README.org

default: 
	@echo Use "make install" when ready
	@echo Or use 'make prepare' then link .emacs and init.el manually

install: prepare
	test -e $(HOME)/.emacs || ln -s $(INSTALLDIR)/.emacs $(HOME)
	test -e $(EMACSHOME)/.init.el || ln -s $(INSTALLDIR)/init.el $(EMACSHOME)
	test -e $(EMACSHOME)/README.org || ln -s $(INSTALLDIR)/README.org $(EMACSHOME)

# ensure key directories and files exist
# and save old customizations
prepare:
	mkdir -p $(EMACSHOME)	# ensure EMACSHOME dir
	mkdir -p $(USER_ME)		# ensure USER_ME dir
	touch $(INIT_ME)			# ensure INIT_ME file
	mkdir -p Limbo			  # ensure Limbo dir
	for f in $(OLD); do test -L $$f || mv -if $$f Limbo; done

whereami:
	@echo $(INSTALLDIR)

TAGS: $(EMACS_SRC)
	etags -o $@ $^
