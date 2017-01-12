all: test

test:
	emacs -batch -L . -l ~/.emacs.d/min-init.el -l test-lbb.el \
-f ert-run-tests-batch-and-exit

install:
	cp -t ~/.emacs.d/site-lisp little-black-book.el
