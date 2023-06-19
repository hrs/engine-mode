;;; engine-mode.el --- Define and query search engines

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 2.2.4
;; URL: https://github.com/hrs/engine-mode
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `engine-mode' is a global minor mode for Emacs. It enables you to easily
;; define search engines, bind them to keybindings, and query them from the
;; comfort of your editor.

;; For example, suppose we want to be able to easily search GitHub:

;; (defengine github
;;   "https://github.com/search?ref=simplesearch&q=%s")

;; This defines an interactive function `engine/search-github'. When executed it
;; will take the selected region (or prompt for input, if no region is selected)
;; and search GitHub for it, displaying the results in your default browser.

;; The `defengine' macro can also take an optional key combination,
;; prefixed with "C-x /":

;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   :keybinding "d")

;; `C-x / d' is now bound to the new function
;; engine/search-duckduckgo'! Nifty.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'format-spec)

(defgroup engine nil
  "Define search engines, bind them to keybindings, and query them."
  :group 'external)

(defcustom engine/keybinding-prefix "C-x /"
  "The default `engine-mode' keybindings prefix."
  :group 'engine
  :type '(choice (string :tag "Key")
                 (const :tag "No keybinding" nil)))

(define-prefix-command 'engine-mode-prefixed-map)
(defvar engine-mode-prefixed-map)

(defvar engine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd engine/keybinding-prefix) engine-mode-prefixed-map)
    map)
  "Keymap for `engine-mode'.")

;;;###autoload
(define-minor-mode engine-mode
  "Minor mode for defining and querying search engines through Emacs.

\\{engine-mode-map}"
  :global t
  :keymap engine-mode-map)

(defun engine/set-keymap-prefix (prefix-key)
  "Bind the engine-mode keymap to PREFIX-KEY.
For example, to use \"C-c s\" instead of the default \"C-x /\":

\(engine/set-keymap-prefix (kbd \"C-c s\"))"
  (when engine/keybinding-prefix
    (define-key engine-mode-map (kbd engine/keybinding-prefix) nil))
  (define-key engine-mode-map prefix-key engine-mode-prefixed-map))

(defcustom engine/browser-function browse-url-browser-function
  "The default browser function used when opening a URL in an engine.
Defaults to `browse-url-browser-function'."
  :group 'engine
  :type 'symbol)

(defun engine--search-prompt (engine-name default-word)
  "Return a search prompt for ENGINE-NAME, defaulting to DEFAULT-WORD."
  (if (string= default-word "")
      (format "Search %s: " (capitalize engine-name))
    (format "Search %s (%s): " (capitalize engine-name) default-word)))

(defun engine--prompted-search-term (engine-name)
  "Prompt the user for a search term for ENGINE-NAME.
Default to the symbol at point."
  (let ((current-word (or (thing-at-point 'symbol 'no-properties) "")))
    (read-string (engine--search-prompt engine-name current-word)
     nil nil current-word)))

(defun engine--get-query (engine-name)
  "Return the selected region or prompt the user for a query for ENGINE-NAME."
  (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
    (engine--prompted-search-term engine-name)))

(defun engine--execute-search (search-engine-url browser-function search-term)
  "Search SEARCH-ENGINE-URL for SEARCH-TERM.
Display the resulting URL with BROWSER-FUNCTION."
  (interactive)
  (let ((browse-url-browser-function browser-function))
    (browse-url
     (format-spec search-engine-url
                  (format-spec-make ?s (url-hexify-string search-term))))))

(defun engine--function-name (engine-name)
  "Return the name of the function for ENGINE-NAME.

For example, if ENGINE-NAME is the symbol `github', return
`engine/search-github'."
  (intern (concat "engine/search-" (downcase (symbol-name engine-name)))))

(defun engine--docstring (engine-name)
  "Construct and return a default docstring for ENGINE-NAME."
  (format "Search %s for the selected text.\nPrompt for input if none is selected."
          (capitalize (symbol-name engine-name))))

(defun engine--bind-key (engine-name keybinding)
  "Bind KEYBINDING to ENGINE-NAME in the `engine-mode-prefixed-map'.

Do nothing if KEYBINDING is nil.

Use `keymap-set' instead of `define-key' if it's available, since
it permits multiple keys in KEYBINDING."
  (when keybinding
    (if (fboundp 'keymap-set)
        `(keymap-set engine-mode-prefixed-map ,keybinding
                     (quote ,(engine--function-name engine-name)))
      `(define-key engine-mode-prefixed-map (kbd ,keybinding)
                   (quote ,(engine--function-name engine-name))))))

;;;###autoload
(cl-defmacro defengine (engine-name search-engine-url
                                    &key keybinding docstring
                                    (browser 'engine/browser-function)
                                    (term-transformation-hook 'identity))
  "Define a custom engine ENGINE-NAME searching SEARCH-ENGINE-URL.

ENGINE-NAME is a symbol naming the engine.

SEARCH-ENGINE-URL is the url to be queried, with a \"%s\"
standing in for the search term.

The optional keyword argument DOCSTRING assigns a docstring to
the generated function. A reasonably sensible docstring will be
generated if a custom one isn't provided.

The optional keyword argument BROWSER assigns the browser
function to be used when opening the URL.

The optional keyword argument TERM-TRANSFORMATION-HOOK is a
function that will be applied to the search term before it's
substituted into SEARCH-ENGINE-URL. For example, if we wanted to
always upcase our search terms, we might use:

\(defengine duckduckgo
  \"https://duckduckgo.com/?q=%s\"
  :term-transformation-hook upcase)

In this case, searching for \"foobar\" will hit the url
\"https://duckduckgo.com/?q=FOOBAR\".

The optional keyword argument KEYBINDING is a string describing
the key to bind the new function.

Keybindings are in `engine-mode-map', so they're prefixed.

For example, to search Wikipedia, use:

  (defengine wikipedia
    \"https://www.wikipedia.org/search-redirect.php?search=%s\"
    :keybinding \"w\"
    :docstring \"Search Wikipedia!\")

Hitting \"C-x / w\" will be bound to the newly-defined
`engine/search-wikipedia' function."
  (declare (indent 1))
  (cl-assert (symbolp engine-name))
  `(prog1
       (defun ,(engine--function-name engine-name) (search-term)
         ,(or docstring (engine--docstring engine-name))
         (interactive
          (list (engine--get-query ,(symbol-name engine-name))))
         (engine--execute-search ,search-engine-url ,browser (,term-transformation-hook search-term)))
     ,(engine--bind-key engine-name keybinding)))

(provide 'engine-mode)
;;; engine-mode.el ends here
