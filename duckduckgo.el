;;; duckduckgo.el --- Emacs interface to the DuckDuckGo Instant Answers API.

;; Copyright (C) 2016, 12pt

;; Author: 12pt
;; URL: https://github.com/12pt/duckduckgo.el
;; Keywords: ddg duckduckgo instant answers

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; You should always be able to find the latest version here at <URL:http://github.com/horsemd/duckduckgo/>.


;; Usage:

;; (require 'duckduckgo)
;; (setq duckduckgo-useragent "my useragent" ; optional
;;       duckduckgo-api-url "https://mycustomurl.com/") ; optional but you probably don't want to change this
;; (duckduckgo-search "Hugh Laurie")
;; (duckduckgo-search "Bacon" t) ; pass t to skip disambiguation

;; duckduckgo-search takes 3 optional parameters: skip disambiguation, no html (which removes html styling),
;; and no redirect (which skips HTTP redirects i.e. for !bang commands).

;;; Code:

(require 'json)
(require 'url)

;; magic 
(defvar url-http-end-of-headers)

(defgroup duckduckgo nil
  "Emacs interface to DuckDuckGo's Instant Answers API."
  :group 'tools)

(defcustom duckduckgo-api-url "https://api.duckduckgo.com/?%s"
  "The API to use when querying DDG. By default it uses https."
  :type '(string)
  :group 'duckduckgo)

(defcustom duckduckgo-useragent "duckduckgo.el"
  "User agent to query the API with."
  :type '(string)
  :group 'duckduckgo)

(defun duckduckgo-response (buff)
  "Extract the JSON response from the request buffer."
  (with-current-buffer buff
    (setq case-fold-search nil)
    (goto-char url-http-end-of-headers)
    (prog1 (json-read)
      (kill-buffer buff))))

(defun clean-up-response (response)
  "Limit the JSON response to the fields in ddg.com/API. Everything is accessible unmodified in 'JSON.
Remove null or empty values from the new alist. Also make 'Type of Instant Answer human readable."

  (let* ((answertypes `(("A" . "article")
                        ("C" . "category")
                        ("D" . "disambiguation")
                        ("E" . "exclusive")
                        ("N" . "name")))
         (trimmed-alist (rassq-delete-all "" (copy-alist response)))

         (type (assoc-default 'Type response))
         (readable-type (assoc-default type answertypes)))
    ;; shadow 'Type with the more human readable version.
    (add-to-list 'trimmed-alist (cons 'Type readable-type))
    (add-to-list 'trimmed-alist (cons 'JSON response))
    trimmed-alist))

(defun duckduckgo-search (query &optional skip-disambiguation no-html no-redirect)
  "Actually perform the search.
TODO: !bang commands break, return a \"json-read: JSON readtable error\"."
  (let ((url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Referer" . ,duckduckgo-useragent)))
        (args `(("q" . ,query)
                ("format" . "json"))))
    (when skip-disambiguation
      (add-to-list 'args (cons "d" "1")))
    (when no-html
      (add-to-list 'args (cons "no_html" "1")))
    (when no-redirect
      (add-to-list 'args (cons "no_redirect" "1")))
    (duckduckgo-response
     (url-retrieve-synchronously
      (format
       duckduckgo-api-url
       (mapconcat (lambda (cons)
                    (format "%s=%s"
                            (url-hexify-string (car cons))
                            (url-hexify-string (cdr cons))))
                  args
                  "&"))))))

(provide 'duckduckgo)
;;; duckduckgo.el ends here
