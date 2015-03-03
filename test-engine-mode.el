(ert-deftest engine-test-alist ()
       "Tests setting `engine-mode-alist"
       (setq engine-mode-alist nil)
       (defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
  "a")
       (defengine duckduckgo
	 "https://duckduckgo.com/?q=%s"
	 nil)
       (should (equal engine-mode-alist
		      '((duckduckgo nil) (amazon "a")))))
