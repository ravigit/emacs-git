;;; zenburn-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Dirk-Jan C. Binnema.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme zenburn
  "Based on the Zenburn theme.")

(custom-theme-set-faces
 'zenburn
 '(default ((t (:background "#3f3f3f" :foreground "#dcdccc"))))
 '(cursor ((t (:background "#e0cf9f" :foreground "#ffffff"))))
 '(region ((t (:background "#5f5f5f"))))
 '(mode-line ((t (:background "#1e2320" :foreground "#acbc90"))))
 '(mode-line-inactive ((t (:background "#2e3330" :foreground "#88b090"))))
 '(fringe ((t (:background "#464646"))))
 '(minibuffer-prompt ((t (:foreground "#e0cf9f" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#8cd0d3" :weight bold))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#999999"))))
 '(font-lock-constant-face ((t (:foreground "#dca3a3" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#8cd0d3"))))
 '(font-lock-keyword-face ((t (:foreground "#e0cf9f" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#cc9393"))))
 '(font-lock-type-face ((t (:foreground "#dfdfbf" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#e0cf9f"))))
 '(font-lock-warning-face ((t (:background "#332323" :foreground "#e37170" :weight bold))))
 '(isearch ((t (:background "#506070" :weight bold))))
 '(lazy-highlight ((t (:background "#1e2320"))))
 '(link ((t (:foreground "#e0cf9f" :underline t))))
 '(link-visited ((t (:foreground "#dfaf8f" :underline t))))
 '(button ((t (:background "#506070" :foreground "#e0cf9f" :weight bold))))
 '(header-line ((t (:background "#2e3330" :foreground "#88b090")))))

(provide-theme 'zenburn)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; zenburn-theme.el  ends here
