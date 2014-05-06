;;; engine-mode.el --- Define and query search engines from within Emacs.

;; Author: Harry R. Schwartz <hello@harryrschwartz.com>
;; Version: 2014.05.06
;; URL: https://github.com/hrs/engine-mode/engine.el

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

;; The `defengine' macro can also take an optional key combination:

;; (defengine duckduckgo
;;   "https://duckduckgo.com/?q=%s"
;;   "C-c / d")

;; `C-c / d' is now bound to the new function
;; qengine/search-duckduckgo'! Nifty.

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

(define-minor-mode engine-mode
  "Minor mode for defining and querying search engines through Emacs."
  :global t
  :keymap (make-sparse-keymap))

(defun engine/search-prompt (engine-name)
  (concat "Search " (capitalize engine-name) ": "))

(defun engine/prompted-search-term (engine-name)
  (read-string (engine/search-prompt engine-name)))

(defun engine/get-query (engine-name)
  "Return the selected region (if any) or prompt the user for a query."
  (if mark-active
      (buffer-substring (region-beginning) (region-end))
    (engine/prompted-search-term engine-name)))

(defun engine/execute-search (search-engine-url search-term)
  "Display the results of the query."
  (interactive)
  (browse-url
   (format search-engine-url
           (url-hexify-string search-term))))

(defun engine/function-name (engine-name)
  (intern (concat "engine/search-" (downcase (symbol-name engine-name)))))

(defun engine/docstring (engine-name)
  (concat "Search "
          (capitalize (symbol-name engine-name))
          " for the selected text. Prompt for input if none is selected."))

(defun engine/bind-key (engine-name keybinding)
  (when keybinding
    `(define-key engine-mode-map (kbd ,keybinding)
       (quote ,(engine/function-name engine-name)))))

(defmacro defengine (engine-name search-engine-url &optional keybinding)
  (let ((evaled-engine-name engine-name)
        (search-term (gensym)))
    `(progn (defun ,(engine/function-name evaled-engine-name) (,search-term)
              ,(engine/docstring evaled-engine-name)
              (interactive
               (list (engine/get-query ,(symbol-name evaled-engine-name))))
              (engine/execute-search ,search-engine-url ,search-term))
            ,(engine/bind-key evaled-engine-name keybinding))))

(provide 'engine-mode)
;;; engine-mode.el ends here
