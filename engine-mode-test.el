;;; engine-mode-test.el --- tests for engine-mode.el

;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for engine-mode.

;;; Code:

(load-file "engine-mode.el")

(ert-deftest engine--function-name ()
  (should (equal (engine--function-name 'wikipedia)
                 'engine/search-wikipedia))
  (should (equal (engine--function-name 'GitHub)
                 'engine/search-github)))

(ert-deftest engine--docstring ()
  (should (equal (engine--docstring 'my-engine)
                 "Search My-Engine for the selected text.\nPrompt for input if none is selected.")))

;;; engine-mode-test.el ends here
