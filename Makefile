.PHONY: test
test: engine-mode.el engine-mode-test.el
	emacs --quick --batch \
	-f package-initialize \
  --eval "(add-to-list 'package-archives '(\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\") t)" \
  --eval "(when (not (package-installed-p 'buttercup)) (package-refresh-contents) (package-install 'buttercup))" \
	-L . \
	-f buttercup-run-discover
