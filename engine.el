;;; engine.el --- Define and query search engines from within Emacs.

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 2015.06.13
;; URL: https://github.com/hrs/engine-mode/engine-mode.el

;; Hacker: Ian Zimmerman <itz@buug.org>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `engine' is a library for Emacs. It enables you to easily define
;; search engines, bind them to keybindings, and query them from the
;; comfort of your editor.

;; For example, suppose we want to be able to easily search GitHub:

;; (defengine github symbol
;;   "https://github.com/search?ref=simplesearch&q=%s")

;; This defines an interactive function `engine-search-github'. When
;; executed it will take the selected region (or prompt for input, if
;; region is not active, defaulting to the symbol near point) and search
;; GitHub for it, displaying the results in your default browser.

;; The `defengine' macro can also take an optional key combination:

;; (defengine duckduckgo symbol
;;   "https://duckduckgo.com/?q=%s"
;;   "d")

;; "d" is now bound to the new function engine-search-duckduckgo in the
;; keymap `engine-map'.

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
(eval-when-compile (require 'easymenu))

(require 'thingatpt)

(defvar engine-map (make-sparse-keymap))

(defvar engine-history nil
  "Input history for engine searches.")

(defconst engine/prompt-fmt "Search %s (default %s): ")

(defconst engine/doc-fmt
  "Search %s for the active region if there is one.
Also search for the region, even if inactive, with a prefix arg USE-REGION.

Otherwise prompt for a term, defaulting to %s near point.")

(easy-menu-define engine-menu global-map "Menu of web search engines."
  '("Websearch"))

(defsubst engine/search-prompt (engine-name default)
  (format engine/prompt-fmt (capitalize (symbol-name engine-name)) default))

(defsubst engine/prompted-search-term (engine-name thing)
  (let ((default (thing-at-point thing)))
    (read-string
     (engine/search-prompt engine-name default)
     nil 'engine-history default)))

(defsubst engine/get-query (engine-name thing use-region)
  "Return the active region (if any) or prompt the user for a query."
  (if (or use-region transient-mark-mode)
      (buffer-substring (region-beginning) (region-end))
    (engine/prompted-search-term engine-name thing)))

(defsubst engine/execute-search (search-term search-engine-url)
  "Display the results of the query."
  (browse-url (format search-engine-url (url-hexify-string search-term))))

(defsubst engine/function-name (engine-name)
  (intern (concat "engine-search-" (downcase (symbol-name engine-name)))))

(defsubst engine/docstring (engine-name thing)
  (format engine/doc-fmt (capitalize (symbol-name engine-name)) thing))

(defsubst engine/bind-key (engine-name keys)
  (when keys
    (let ((engfun (engine/function-name engine-name)))
      (define-key engine-map keys `(function ,engfun)))))

(defsubst engine/add-item (engine-name)
  (let* ((engfun (engine/function-name engine-name))
         (item (vector (capitalize (symbol-name engine-name)) engfun t)))
    (easy-menu-add-item engine-menu nil item)))

(defsubst engine/cpa () current-prefix-arg)

(defmacro defengine (engine-name thing url &optional keys xform docstring)
  "Define a custom search engine.

ENGINE-NAME is a symbol naming the engine.  URL is the url to be
queried, with a \"%s\" standing in for the search term.  The
search term itself is the region in transient mark mode or with a
prefix arg, otherwise it is obtained interactively via the
minibuffer, with a default based on THING around point.

The optional argument DOCSTRING assigns a docstring to
the generated function. A reasonably sensible docstring will be
generated if a custom one isn't provided.  The optional
argument XFORM is a function that will be applied to the
search term before it's substituted into URL. For example, if we
wanted to always upcase our search terms, we might use:

  (defengine duckduckgo symbol
    \"https://duckduckgo.com/?q=%s\" \"d\" 'upcase)

In this case, searching for \"foobar\" will hit the url
\"https://duckduckgo.com/?q=FOOBAR\".

The optional argument KEYS is a string describing
the key or key sequence to bind the new function.
Keybindings are in the `engine-map'."

  (let ((en engine-name))
    `(prog1
         (defun ,(engine/function-name en) (term &optional use-region)
           ,(or docstring (engine/docstring en thing))
           (interactive
            (list (engine/get-query (quote ,en) (quote ,thing) (engine/cpa))))
           (engine/execute-search (funcall (or ,xform 'identity) term) ,url))
       (engine/add-item (quote ,en))
       (engine/bind-key (quote ,en) ,keys))))

(provide 'engine)
;;; engine.el ends here
