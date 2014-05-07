# engine-mode

`engine-mode` is a global minor mode for Emacs. It enables you to
easily define search engines, bind them to keybindings, and query them
from the comfort of your editor.

For example, suppose we want to be able to easily search GitHub:

```emacs
(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")
```

This defines an interactive function `engine/search-github`. When
executed it will take the selected region (or prompt for input, if no
region is selected) and search GitHub for it, displaying the results
in your default browser.

The `defengine` macro can also take an optional key combination:

```emacs
(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  "C-c / d")
```

`C-c / d` is now bound to the new function `engine/search-duckduckgo`!
Nifty.

## Installation

`engine-mode` is available on MELPA.

You can also install it like any other elisp file by adding it to your
load path and globally enabling it:

```emacs
(require 'engine-mode)
(engine-mode t)
```

## Examples

```emacs
(defengine amazon
  "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  "C-c / d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  "C-c / g")

(defengine google-images
  "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

(defengine google-maps
  "http://maps.google.com/maps?q=%s")

(defengine project-gutenberg
  "http://www.gutenberg.org/ebooks/search.html/?format=html&default_prefix=all&sort_order=&query=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine twitter
  "https://twitter.com/search?q=%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  "C-c / w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(defengine wolfram-alpha
  "http://www.wolframalpha.com/input/?i=%s")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
```
