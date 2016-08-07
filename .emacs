;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (>= emacs-major-version 24)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/")
                           )))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

;; Set the following to true (t) when moving config to a new machine
(defvar reload-packages nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (if reload-packages						        ;;
;;     (package-refresh-contents)				        ;;
;;   (when (not (package-installed-p 'save-packages))		        ;;
;;     (package-install 'save-packages))			        ;;
;;   ;; Restore all the saved packages from the index.		        ;;
;;   ;; The following poses an interactive question, wether or not      ;;
;;   ;; to install a package. To say yes for all, choose the option '!' ;;
;;   (install-saved-packages))					        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;
;; Set and LoadPaths  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "/usr/local/bin")
(let ((load-dirs '("site-lisp")))
  (mapcar (lambda(dir)
            (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Required Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-pkgs
  (list 'auto-complete-config 'autopair 'butler 'emacs-os
        'ein 'google 'grep-buffers 'helm-config 'hide-lines
        'highlight-indentation 'js2-mode 'language-styles
        'misc 'moinmoin-mode 'nexus 'org-install
        'pytest 'revbufs 'save-packages 'show-functions
        'xub-mode 'xub-mode 'transpose-frame 'yasnippet))

(mapcar 'require local-pkgs)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set mode parameters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(autopair-mode)
(column-number-mode t)
(desktop-save-mode 1)
(fringe-mode)
(global-hi-lock-mode 1)
(global-linum-mode t)
(scroll-bar-mode -1)
;; (semantic-mode 1)
(setq scroll-step 1)
(show-paren-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(which-function-mode t)
(yas-reload-all)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set variables that control emacs behavior ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-config-default)
(setq python-indent-offset 3)
(setq ring-bell-function #'ignore)
(setq scroll-down-aggressively 0.0)
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add mode bindings to file types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.gradle$" .     groovy-mode))
(add-to-list 'auto-mode-alist '("\\.js$" .         js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" .        java-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" .   markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" .         markdown-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" .        clojure-mode))


;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;;;;;;;;;;;;;;;
;; Add hooks ;;
;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'clojure-mode-hook (lambda() (enable-paredit-mode)))
(add-hook 'clojure-mode-hook (lambda() (rainbow-delimiters-mode)))
(add-hook 'clojure-mode-hook (lambda()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1)
                               (cljr-add-keybindings-with-prefix "s-=")))
(add-hook 'markdown-mode-hook (lambda() (flyspell-mode 1)))
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(add-hook 'java-mode-hook #'yas-minor-mode)
;; (add-hook 'python-mode-hook (lambda() (highlight-indentation-mode)))


(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

;;;;;;;;;;;;;;;
;; Key maps  ;;
;;;;;;;;;;;;;;;
(let ((mappings (list
                 '("\C-\\"     uncomment-line-or-region)
                 '("\C-c'"     gnus)
                 '("\C-c."     add-to-imports)
                 '("\C-c/"     comment-line-or-region)
                 '("\C-c0"     bm-show-all)
                 '("\C-c1"     bm-toggle)
                 '("\C-c2"     bm-next)
                 '("\C-c3"     bm-previous)
                 '("\C-c;"     search-index)
                 '("\C-c?"     open-org)
                 '("\C-c\C-b"  search-forward-regexp)
                 '("\C-c\C-t"  restart-tomcat)
                 '("\C-ca"     open-fileline)
                 '("\C-cb"     grep-buffers)
                 '("\C-cc"     compile)
                 '("\C-cd"     vc-diff)
                 '("\C-ce"     blank-mode)
                 '("\C-cf"     grep)
                 '("\C-cg"     google-it)
                 '("\C-ch"     hide-lines)
                 '("\C-ci"     transpose-frame)
                 '("\C-cj"     join-line)
                 '("\C-ck"     tags-apropos)
                 '("\C-cl"     cider-connect) ; clojure uses cider
                 '("\C-cn"     auto-revert-tail-mode)
                 '("\C-co"     eval-buffer)
                 '("\C-cp"     goto-line)
                 '("\C-cq"     query-replace)
                 '("\C-crf"    recursive-grep)
                 '("\C-cs"     cscope-find-global-definition)
                 '("\C-cu"     my-browse-url)
                 '("\C-cv"     indent-buffer)
                 '("\C-cy"     duplicate-line)
                 '("\C-cx"     replace-string)
                 '("\C-cz"     close-all-buffers)
                 '("\M-["      beginning-of-defun)
                 '("\M-]"      end-of-defun)
                 '([f1]        search-forward-regexp)
                 '([f2]        search-backward-regexp)
                 '([f3]        xah-emacs-help)
                 '([f5]        search-index)
                 )))
  (mapcar (lambda (mapping)
            (let ((key (car mapping))
                  (func (cadr mapping)))
              (progn
                (message (format "map key %s to %s" key func))
                (global-set-key key func))))
          mappings))


(defun disable-themes ()
  (interactive)
  "Disable all loaded themes"
  (mapcar 'disable-theme custom-enabled-themes))


(global-set-key (kbd "s-a") 'package-list-packages)
(global-set-key (kbd "s-c") 'cua-mode)
(global-set-key (kbd "s-b") 'indent-buffer)
(global-set-key (kbd "s-p") 'paredit-mode)
(global-set-key (kbd "s-r") 'replace-regexp)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-l") 'load-theme)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-v") 'magit-diff)
(global-set-key (kbd "s-n") 'magit-blame)
(global-set-key (kbd "s-/") 'comment-line-or-region)
(global-set-key (kbd "s-\\") 'uncomment-line-or-region)
(global-set-key (kbd "s-d") 'disable-themes)
(global-set-key (kbd "s-w") 'whack-whitespace)

;; (define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
 This prevents overlapping themes; something I would rarely want."
  (mapcar 'disable-theme custom-enabled-themes))

(ad-activate 'load-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inherit shell environment on OS X ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq window-system '(mac ns))
  (exec-path-from-shell-copy-env "DYLD_LIBRARY_PATH")
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;
;; Misc functions  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (progn
        (save-packages)
        (save-buffers-kill-emacs))))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)


;;;;;;;;;;;;;;;;;;
;; Emacs Server ;;
;;;;;;;;;;;;;;;;;;

(server-start)


(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3f3f3f" "#ea3838" "#7fb07f" "#fe8b04" "#62b6ea" "#e353b9" "#1fb3b3" "#d5d2be"])
 '(column-number-mode t)
 '(cua-mode nil nil (cua-base))
 '(custom-safe-themes
   (quote
    ("12722541c8998f056b761bf63a92216aaf4610e4eb1afe7991842a31fa28b6d8" "cee3ced547529a0923830318df23cb329255b963e39951d79fd7e56c54b0ade3" "c4a784404a2a732ef86ee969ab94ec8b8033aee674cd20240b8addeba93e1612" "2f5dd0ac7dffdc0acf0aa15c9b7a5b1f86c37b9e11800325160b89c1b8a6fefe" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#222222")
 '(fringe-mode 10 nil (fringe))
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(linum-format " %5i ")
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face))
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t)))
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (quote
    ((20 . "#fa5151")
     (40 . "#ea3838")
     (60 . "#f8ffa0")
     (80 . "#e8e815")
     (100 . "#fe8b04")
     (120 . "#e5c900")
     (140 . "#32cd32")
     (160 . "#8ce096")
     (180 . "#7fb07f")
     (200 . "#3cb370")
     (220 . "#099709")
     (240 . "#2fdbde")
     (260 . "#1fb3b3")
     (280 . "#8cf1f1")
     (300 . "#94bff3")
     (320 . "#62b6ea")
     (340 . "#00aff5")
     (360 . "#e353b9"))))
 '(vc-annotate-very-old-color "#e353b9"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
