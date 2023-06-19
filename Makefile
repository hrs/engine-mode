.PHONY: test
test: engine-mode.el engine-mode-test.el
	emacs --quick --batch \
	-f package-initialize \
	-L . \
	-f buttercup-run-discover
