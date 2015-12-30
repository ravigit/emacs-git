;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

;; Set the following to true (t) when moving config to a new machine
(defvar reload-packages nil)

(if reload-packages
    (package-refresh-contents)
  (when (not (package-installed-p 'save-packages))
    (package-install 'save-packages))
  ;; Restore all the saved packages from the index.
  ;; The following poses an interactive question, wether or not
  ;; to install a package. To say yes for all, choose the option '!'
  (install-saved-packages))


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
        'google 'grep-buffers 'helm-config 'hide-lines
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


;;;;;;;;;;;;;;;
;; Add hooks ;;
;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'clojure-mode-hook (lambda() (enable-paredit-mode)))
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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'butler-server-list
             '(jenkins "jenkins1"
                       (server-address . "https://jenkins1")
                       (auth-file . "~/.authinfo.gpg")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs managed variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(Linum-format "%7i ")
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
;;  '(ansi-term-color-vector
;;    [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"] t)
;;  '(column-number-mode t)
;;  '(compilation-message-face (quote default))
;;  '(cua-global-mark-cursor-color "#2aa198")
;;  '(cua-normal-cursor-color "#839496")
;;  '(cua-overwrite-cursor-color "#b58900")
;;  '(cua-read-only-cursor-color "#859900")
;;  '(custom-safe-themes
;;    (quote
;;     ("c35c0effa648fd320300f3d45696c640a92bdc7cf0429d002a96bda2b42ce966" default)))
;;  '(ecb-options-version "2.32")
;;  '(ensime-sem-high-faces
;;    (quote
;;     ((var :foreground "#9876aa" :underline
;;           (:style wave :color "yellow"))
;;      (val :foreground "#9876aa")
;;      (varField :slant italic)
;;      (valField :foreground "#9876aa" :slant italic)
;;      (functionCall :foreground "#a9b7c6")
;;      (implicitConversion :underline
;;                          (:color "#808080"))
;;      (implicitParams :underline
;;                      (:color "#808080"))
;;      (operator :foreground "#cc7832")
;;      (param :foreground "#a9b7c6")
;;      (class :foreground "#4e807d")
;;      (trait :foreground "#4e807d" :slant italic)
;;      (object :foreground "#6897bb" :slant italic)
;;      (package :foreground "#cc7832")
;;      (deprecated :strike-through "#a9b7c6"))))
;;  '(fci-rule-character-color "#202020")
;;  '(fci-rule-color "#202020")
;;  '(frame-brackground-mode (quote dark))
;;  '(fringe-mode 6 nil (fringe))
;;  '(helm-mode t)
;;  '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
;;  '(highlight-symbol-colors
;;    (--map
;;     (solarized-color-blend it "#002b36" 0.25)
;;     (quote
;;      ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
;;  '(highlight-symbol-foreground-color "#93a1a1")
;;  '(highlight-tail-colors
;;    (quote
;;     (("#F2F2F2" . 0)
;;      ("#B4C342" . 20)
;;      ("#69CABF" . 30)
;;      ("#6DA8D2" . 50)
;;      ("#DEB542" . 60)
;;      ("#F2804F" . 70)
;;      ("#F771AC" . 85)
;;      ("#F2F2F2" . 100))))
;;  '(hl-bg-colors
;;    (quote
;;     ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
;;  '(hl-fg-colors
;;    (quote
;;     ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
;;  '(jabber-account-list
;;    (quote
;;     (("ravi@jabber.locationlabs.com"
;;       (:network-server . "jabber.locationlabs.com")
;;       (:port . 5222)
;;       (:connection-type . starttls))
;;      ("gorrepati@gmail.com"
;;       (:network-server . "talk.google.com")
;;       (:port . 5223)
;;       (:connection-type . ssl)))))
;;  '(jabber-alert-message-hooks
;;    (quote
;;     (jabber-message-screen jabber-message-echo jabber-message-scroll)))
;;  '(jabber-alert-presence-hooks nil)
;;  '(jabber-chat-buffer-show-avatar nil)
;;  '(jabber-default-status "DONT PANIC!")
;;  '(jabber-display-menu t)
;;  '(jabber-history-enabled t)
;;  '(jabber-history-size-limit 1024000)
;;  '(jabber-muc-autojoin (quote ("cni@conference.jabber.locationlabs.com")))
;;  '(jabber-roster-default-group-name nil)
;;  '(jabber-use-global-history t)
;;  '(jabber-vcard-avatars-retrieve nil)
;;  '(linum-format "%d")
;;  '(magit-diff-use-overlays nil)
;;  '(magit-use-overlays nil)
;;  '(main-line-color1 "#191919")
;;  '(main-line-color2 "#111111")
;;  '(main-line-separator-style (quote chamfer))
;;  '(menu-bar-mode nil)
;;  '(nexus-rest-url "http://nexus1/service/local/lucene/search")
;;  '(pos-tip-background-color "#073642")
;;  '(pos-tip-foreground-color "#93a1a1")
;;  '(powerline-color1 "#191919")
;;  '(powerline-color2 "#111111")
;;  '(pyvirtualenv-mode t)
;;  '(safe-local-variable-values
;;    (quote
;;     ((checkdoc-minor-mode . t)
;;      (mangle-whitespace . t)
;;      (nxml-attribute-indent . 2)
;;      (nxml-child-indent . 2)
;;      (nxml-attribute-indent . 3)
;;      (nxml-child-indent . 3))))
;;  '(scroll-bar-mode nil)
;;  '(send-mail-function (quote smtpmail-send-it))
;;  '(show-paren-mode t)
;;  '(size-indication-mode t)
;;  '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
;;  '(smtpmail-smtp-server "webmail.locationlabs.com")
;;  '(smtpmail-smtp-service 587)
;;  '(tool-bar-mode nil)
;;  '(vc-annotate-background "#2b2b2b")
;;  '(vc-annotate-color-map
;;    (quote
;;     ((20 . "#bc8383")
;;      (40 . "#cc9393")
;;      (60 . "#dfaf8f")
;;      (80 . "#d0bf8f")
;;      (100 . "#e0cf9f")
;;      (120 . "#f0dfaf")
;;      (140 . "#5f7f5f")
;;      (160 . "#7f9f7f")
;;      (180 . "#8fb28f")
;;      (200 . "#9fc59f")
;;      (220 . "#afd8af")
;;      (240 . "#bfebbf")
;;      (260 . "#93e0e3")
;;      (280 . "#6ca0a3")
;;      (300 . "#7cb8bb")
;;      (320 . "#8cd0d3")
;;      (340 . "#94bff3")
;;      (360 . "#dc8cc3"))))
;;  '(vc-annotate-very-old-color "#dc8cc3")
;;  '(weechat-color-list
;;    (quote
;;     (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
;;  '(xterm-color-names
;;    ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
;;  '(xterm-color-names-bright
;;    ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
;;  '(zen-encumbered-urls
;;    (quote
;;     ("#brief timewastes" "www.penny-arcade.com" "www.dilbert.com" "www.xkcd.com" "www.userfriendly.org" "#news waste" "slashdot.org" "dn.se" "#social timewastes" "https://www.facebook.com")))
;;  '(zen-fullscreen-mode t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (put 'erase-buffer 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; (mapcar 'disable-theme custom-enabled-themes)
;; (set-background-color "white smoke")
;; (set-foreground-color "black")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "726dd9a188747664fbbff1cd9ab3c29a3f690a7b861f6e6a1c64462b64b306de" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b6f42c69cf96795c75b1e79e5cd8ca62f9f9a0cb07bf11d1e0b49f97785358f1" "c35c0effa648fd320300f3d45696c640a92bdc7cf0429d002a96bda2b42ce966" default)))
 '(linum-format " %3i "))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
