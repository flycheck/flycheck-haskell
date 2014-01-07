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
PACKAGE_SRCS = $(SRCS) $(HELPER_SRCS)
PACKAGE = flycheck-haskell-$(VERSION).tar

.PHONY: compile
compile : $(OBJECTS)

.PHONY: package
package : $(PACKAGE)

$(PACKAGE) : $(PACKAGE_SRCS)
	rm -rf flycheck-haskell-$(VERSION)
	mkdir -p flycheck-haskell-$(VERSION)
	cp -f $(PACKAGE_SRCS) flycheck-haskell-$(VERSION)
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
