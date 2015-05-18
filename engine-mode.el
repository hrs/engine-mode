;;; engine-mode.el --- Define and query search engines from within Emacs.

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 2015.05.02
;; URL: https://github.com/hrs/engine-mode/engine-mode.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `engine-mode' is a global minor mode for Emacs. It enables you to
;; easily define search engines, bind them to keybindings, and query
;; them from the comfort of your editor.

;; For example, suppose we want to be able to easily search GitHub:

;; (defengine github
;;   "https://github.com/search?ref=simplesearch&q=%s")

;; This defines an interactive function `engine/search-github'. When
;; executed it will take the selected region (or prompt for input, if
;; no region is selected) and search GitHub for it, displaying the
;; results in your default browser.

;; The `defengine' macro can also take an optional key combination,
;; prefixed with `engine/keymap-prefix' (which defaults to "C-c /"):

;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   :keybinding "d")

;; `C-c / d' is now bound to the new function
;; engine/search-duckduckgo'! Nifty.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(eval-when-compile (require 'cl))

(define-minor-mode engine-mode
  "Minor mode for defining and querying search engines through Emacs.

\\{engine-mode-map}"
  :global t
  :keymap (make-sparse-keymap))

(defcustom engine/keymap-prefix (kbd "C-c /")
  "Engine-mode keymap prefix."
  :group 'engine-mode
  :type 'string)

(defcustom engine/browser-function browse-url-browser-function
  "The browser function used in call to `browse-url'.
By default it is the same as `browse-url-browser-function'."
  :group 'engine-mode
  :type 'symbol)

(defun engine/search-prompt (engine-name)
  (concat "Search " (capitalize engine-name) ": "))

(defun engine/prompted-search-term (engine-name)
  (read-string (engine/search-prompt engine-name) (thing-at-point 'symbol)))

(defun engine/get-query (engine-name)
  "Return the selected region (if any) or prompt the user for a query."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (engine/prompted-search-term engine-name)))

(defun engine/execute-search (search-engine-url search-term)
  "Display the results of the query."
  (interactive)
  (let ((browse-url-browser-function engine/browser-function))
    (browse-url
     (format search-engine-url
             (url-hexify-string search-term)))))

(defun engine/function-name (engine-name)
  (intern (concat "engine/search-" (downcase (symbol-name engine-name)))))

(defun engine/docstring (engine-name)
  (concat "Search "
          (capitalize (symbol-name engine-name))
          " for the selected text. Prompt for input if none is selected."))

(defun engine/scope-keybinding (keybinding)
  (concat engine/keymap-prefix " " keybinding))

(defun engine/bind-key (engine-name keybinding)
  (when keybinding
    `(define-key engine-mode-map (kbd ,(engine/scope-keybinding keybinding))
       (quote ,(engine/function-name engine-name)))))

(cl-defmacro defengine (engine-name search-engine-url &key keybinding docstring (term-transformation-hook 'identity))
  "Define a custom search engine.

`engine-name' is a symbol naming the engine.
`search-engine-url' is the url to be queried, with a \"%s\"
standing in for the search term.
The optional keyword argument `docstring' assigns a docstring to
the generated function. A reasonably sensible docstring will be
generated if a custom one isn't provided.

The optional keyword argument `term-transformation-hook' is a
function that will be applied to the search term before it's
substituted into `search-engine-url'. For example, if we wanted
to always upcase our search terms, we might use:

(defengine duckduckgo
  \"https://duckduckgo.com/?q=%s\"
  :term-transformation-hook 'upcase)

In this case, searching for \"foobar\" will hit the url
\"https://duckduckgo.com/?q=FOOBAR\".

The optional keyword argument `keybinding' is a string describing
the key to bind the new function.

Keybindings are prefixed by the `engine/keymap-prefix', which
defaults to `C-c /'.

For example, to search Wikipedia, use:

  (defengine wikipedia
    \"http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s\"
    :keybinding \"w\"
    :docstring \"Search Wikipedia!\")

Hitting \"C-c / w\" will be bound to the newly-defined
`engine/search-wikipedia' function."

  (assert (symbolp engine-name))
  `(prog1
     (defun ,(engine/function-name engine-name) (search-term)
       ,(or docstring (engine/docstring engine-name))
       (interactive
        (list (engine/get-query ,(symbol-name engine-name))))
       (engine/execute-search ,search-engine-url (,term-transformation-hook search-term)))
     ,(engine/bind-key engine-name keybinding)))

(provide 'engine-mode)
;;; engine-mode.el ends here
