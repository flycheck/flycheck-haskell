EMACS = emacs
EMACSFLAGS =
CASK = cask
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = flycheck-haskell.el
OBJECTS = $(SRCS:.el=.elc)
HELPER_SRCS = helpers/get-source-directories.hs
PACKAGE = flycheck-haskell-$(VERSION).tar

.PHONY: compile
compile : $(OBJECTS)

.PHONY: package
package : $(PACKAGE)

$(PACKAGE) : $(SRCS) $(HELPER_SRCS)
	rm -rf flycheck-haskell-$(VERSION)
	mkdir -p flycheck-haskell-$(VERSION)
	mkdir -p flycheck-haskell-$(VERSION)/helpers
	cp -f $(SRCS) flycheck-haskell-$(VERSION)
	cp -f $(HELPER_SRCS) flycheck-haskell-$(VERSION)/helpers
	tar cf $(PACKAGE) flycheck-haskell-$(VERSION)
	rm -rf flycheck-haskell-$(VERSION)

.PHONY: clean-all
clean-all : clean clean-pkgdir

.PHONY: clean
clean :
	rm -rf $(OBJECTS)
	rm -rf flycheck-haskell-*.tar flycheck-haskell-pkg.el

.PHONY: packages
packages : $(PKGDIR)

.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

flycheck-haskell-pkg.el : Cask
	$(CASK) package
