EMACS_D_SRC = $(wildcard *.el)
EMACS_JGD_SRC = $(wildcard JGD/*.el)
EMACS_NGENDER_SRC = $(wildcard NGender/*.el)
EMACS_SRC = $(EMACS_D_SRC) $(EMACS_JGD_SRC) $(EMACS_NGENDER_SRC)
TAGS: $(EMACS_SRC)
	etags -o $@ $^
