;;; engine-mode-test.el --- tests for engine-mode.el

;; -*- lexical-binding: t; -*-

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
                        (push-mark-command (point))
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

;;; engine-mode-test.el ends here
