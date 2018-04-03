EMACS ?= emacs

ELS = site-lisp/quelpa/quelpa.el site-lisp/exec-path-from-shell/exec-path-from-shell.el
ELCS = $(ELS:.el=.elc)

all: site-autoload elc elpa

%.elc: %.el
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<

elc: $(ELCS)

elpa: my-packages.el
	$(EMACS) -batch -l my-packages.el

site-autoload: $(ELS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
           (require 'package) \
	   (package-generate-autoloads \"site-lisp\" (locate-user-emacs-file \"site-lisp\")))"

clean:
	@rm $(ELCS) site-lisp/site-lisp-autoloads.el

.PHONY: all clean
