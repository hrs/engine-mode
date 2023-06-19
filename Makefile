.PHONY: test
test: engine-mode.el engine-mode-test.el
	emacs --quick --batch \
	-l ert \
	-l engine-mode-test.el \
	--eval "(setq ert-batch-backtrace-right-margin 10000)" \
	-f ert-run-tests-batch-and-exit
