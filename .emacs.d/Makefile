EMACS ?= emacs

ELS = init.el site-lisp/my.el site-lisp/php-eldoc.el site-lisp/pixiv-dev.el
ELCS = $(ELS:.el=.elc)

all: site-autoload elpa elc

%.elc: %.el
	$(EMACS) -Q -batch -L . -f package-initialize -f batch-byte-compile $<

elc: $(ELCS)

elpa: my-packages.el
	$(EMACS) -batch -l my-packages.el

site-autoload: $(ELS)
	$(EMACS) -Q -batch -L site-lisp --eval \
	"(progn \
           (require 'package) \
	   (package-generate-autoloads \"site-lisp\" (locate-user-emacs-file \"site-lisp\")))"

clean:
	@rm $(ELCS) site-lisp/site-lisp-autoloads.el

.PHONY: all clean elc elpa
