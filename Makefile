EMACS = emacs
EMACSFLAGS =
CASK = cask
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck-haskell.el
OBJECTS = $(SRCS:.el=.elc)
HELPER_SRCS = helpers/get-source-directories.hs
PACKAGE = flycheck-haskell-$(VERSION).tar

.PHONY: compile dist \
	clean clean-elc clean-dist clean-deps \
	deps

# Build targets
compile : $(OBJECTS)

dist :
	$(CASK) package

# Support targets
deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc clean-dist clean-deps

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf $(DISTDIR)

clean-deps :
	rm -rf $(PKGDIR)

# File targets
%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)
