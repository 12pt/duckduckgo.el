# duckduckgo.el

Emacs interface to DuckDuckGo's Instant Answers API.

## Installation

I'll upload it when it's worth uploading.

## Configuration

```emacs-lisp
(setq duckduckgo-useragent "my useragent" ; optional
      duckduckgo-api-url "https://mycustomurl.com/") ; optional but you probably never want to change this
```

## Usage

Function `duckduckgo-search` takes 3 optional parameters: skip disambiguation, no html (which removes 
html styling), and no redirect (which skips HTTP redirects i.e. for !bang commands).

```emacs-lisp
(duckduckgo-search "Hugh Laurie")
(duckduckgo-search "Bacon" t) ; pass t to skip disambiguation
```

!bang redirects:

```emacs-lisp
(let ((search (duckduckgo-search "!yt pepsi bottle, a coca cola glass" nil nil t)))
  (assoc 'Redirect search))
```
