EMACS = emacs
EMACSFLAGS =
GHC = ghc
GHCFLAGS = -Wall -Werror -O1
CASK = cask
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

EL_SRCS = flycheck-haskell.el
EL_OBJS = $(EL_SRCS:.el=.elc)
HS_SRCS = get-cabal-configuration.hs
PACKAGE = flycheck-haskell-$(VERSION).tar

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist \
	test \
	clean clean-elc clean-dist clean-deps \
	deps

# Build targets
compile : $(EL_OBJS)

dist :
	$(CASK) package

# Test targets
test : $(EL_OBJS)
	$(CASK) exec $(EMACSBATCH) -l flycheck-haskell.elc \
		-l test/flycheck-haskell-test.el -f ert-run-tests-batch-and-exit

# Support targets
deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc clean-hs clean-dist clean-deps

clean-elc :
	rm -rf $(EL_OBJS)

clean-dist :
	rm -rf $(DISTDIR)

clean-deps :
	rm -rf $(PKGDIR)

# File targets
%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)
