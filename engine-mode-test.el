;;; engine-mode-test.el --- tests for engine-mode.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for engine-mode.

;;; Code:

(require 'buttercup)
(load-file "engine-mode.el")

(describe "engine--search-prompt"
          (it "includes the default term in the prompt"
              (expect (engine--search-prompt "wikipedia" "the-thing-at-point")
                      :to-equal
                      "Search Wikipedia (the-thing-at-point): "))

          (it "doesn't include the default term if it's empty"
              (expect (engine--search-prompt "wikipedia" "")
                      :to-equal
                      "Search Wikipedia: ")))

(describe "engine--prompted-search-term"
          (it "reads a string from the user"
              (spy-on 'read-string :and-return-value "spy term")
              (spy-on 'thing-at-point :and-return-value nil)

              (expect (engine--prompted-search-term "wikipedia")
                      :to-equal "spy term")
              (expect 'read-string :to-have-been-called-with
                      (engine--search-prompt "wikipedia" "") nil nil ""))

          (it "defaults the term to the thing-at-point, if there is one"
              (spy-on 'read-string :and-return-value "spy term")
              (with-temp-buffer
                (insert "foo")
                (goto-char 1)
                (expect (engine--prompted-search-term "wikipedia")
                        :to-equal "spy term"))

              (expect 'read-string :to-have-been-called-with
                      (engine--search-prompt "wikipedia" "foo") nil nil "foo")))

(describe "engine--get-query"
          (it "returns a region, if one's active"
              (expect (with-temp-buffer
                        (transient-mark-mode)

                        (insert "lorem ipsum")
                        (goto-char 3)
                        (push-mark-command (point) t)
                        (goto-char 10)

                        (engine--get-query "wikipedia"))
                      :to-equal "rem ips"))

          (it "delegates to engine--prompted-search-term if there's no active region"
              (spy-on 'read-string :and-return-value "spy term")
              (expect (engine--get-query "wikipedia")
                      :to-equal
                      (engine--prompted-search-term "wikipedia"))))

(describe "engine--execute-search"
          (it "encodes the search term, interpolates it into the URL, and browses it"
              (expect (engine--execute-search
                       "https://www.wikipedia.org/search-redirect.php?search=%s"
                       (lambda (url &rest _) (cons 'browsed url))
                       "foo bar")
                      :to-equal
                      '(browsed . "https://www.wikipedia.org/search-redirect.php?search=foo%20bar"))))

(describe "engine--function-name"
          (it "returns the function name created by the engine with this name"
              (expect (engine--function-name 'wikipedia)
                      :to-equal
                      'engine/search-wikipedia))

          (it "downcases the engine name"
              (expect (engine--function-name 'GitHub)
                      :to-equal
                      'engine/search-github)))

(describe "engine--docstring"
          (it "returns a default docstring with the engine name interpolated"
              (expect (engine--docstring 'my-engine)
                      :to-equal
                      "Search My-Engine for the selected text.\nPrompt for input if none is selected.")))

(defun expect-binding (function-name desc)
  "Test that FUNCTION-NAME has keybinding DESC."
  (expect (member desc
                  (mapcar #'key-description (where-is-internal function-name engine-mode-map)))
          :not :to-be nil))

(describe "defengine"
          (describe ":keybinding"
                    (before-each
                     ;; Restore keymaps to their default values
                     (setq engine-mode-prefixed-map (make-sparse-keymap))
                     (setq engine-mode-map
                           (let ((map (make-sparse-keymap)))
                             (define-key map (kbd engine/keybinding-prefix) engine-mode-prefixed-map)
                             map)))

                    (it "binds a key with the default prefix"
                        (defengine wikipedia "" :keybinding "w")

                        (expect-binding 'engine/search-wikipedia "C-x / w"))

                    (it "binds a key with a custom prefix"
                        (engine/set-keymap-prefix (kbd "C-c s"))

                        (defengine wikipedia "" :keybinding "w")

                        (expect-binding 'engine/search-wikipedia "C-c s w"))

                    (it "binds sequential keys with the default prefix"
                        (defengine wikipedia "" :keybinding "w o w")

                        (expect-binding 'engine/search-wikipedia "C-x / w o w"))

                    (it "binds sequential keys with a custom prefix"
                        (engine/set-keymap-prefix (kbd "C-c s"))

                        (defengine wikipedia "" :keybinding "w o w")

                        (expect-binding 'engine/search-wikipedia "C-c s w o w"))

                    (it "rebinds the keymap even after engines are defined"
                        (defengine wikipedia "" :keybinding "w")

                        (engine/set-keymap-prefix (kbd "C-c s"))

                        (expect-binding 'engine/search-wikipedia "C-c s w")))

          (describe ":term-transformation-hook"
                    (it "applies the hook to search terms"
                        (defengine wikipedia
                          "https://www.wikipedia.org/search-redirect.php?search=%s"
                          :term-transformation-hook upcase)

                        (let ((engine/browser-function (lambda (url &rest _) url)))
                          (expect (engine/search-wikipedia "foo bar")
                                  :to-equal
                                  "https://www.wikipedia.org/search-redirect.php?search=FOO%20BAR")))))

;;; engine-mode-test.el ends here
