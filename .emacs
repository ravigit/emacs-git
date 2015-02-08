
;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(package-refresh-contents)

(when (not (package-installed-p 'save-packages))
  (package-install 'save-packages))

(install-saved-packages)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Set and LoadPaths  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "/usr/local/bin")
(let ((load-dirs '("site-lisp")))
  (mapcar (lambda(dir)
            (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs))

;;;;;;;;;;;;;;;;;;;
;; Load Packages ;;
;;;;;;;;;;;;;;;;;;;

(let ((local-pkgs (list
                   'auto-complete-config
                   'autopair
                   'butler
                   'emacs-os
                   'google
                   'grep-buffers
                   'hide-lines
                   'js2-mode
                   'language-styles
                   'misc
                   'moinmoin-mode
                   'nexus
                   'nsi-mode
                   'org-install
                   'psvn
                   'revbufs
                   'save-packages
                   'show-functions
                   'xub-mode
                   'xub-mode
                   'transpose-frame
                   )))
  (mapcar 'require local-pkgs))


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
(semantic-mode 1)
(setq scroll-step 1)
(show-paren-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(which-function-mode t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set variables that control emacs behavior ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ac-config-default)
(setq python-indent-offset 3)
(setq rcirc-server-alist  '(("bugz" :channels  ("#engr"))))
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
(add-to-list 'auto-mode-alist '("\\.nsh$" .        nsi-mode))
(add-to-list 'auto-mode-alist '("\\.nsi$" .        nsi-mode))



;;;;;;;;;;;;;;;
;; Add hooks ;;
;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda () (load "dired-x")))



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



;;;;;;;;;;;;;;;;;;;;;;
;; Add transparency ;;
;;;;;;;;;;;;;;;;;;;;;;
(let ((active-transparency 98)
      (inactive-transparency 98))
  (set-frame-parameter
   (selected-frame)
   'alpha
   (list active-transparency  inactive-transparency)))




;;;;;;;;;;;;;;;;;;;;;
;; Misc functions  ;;
;;;;;;;;;;;;;;;;;;;;;
(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)



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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(ansi-term-color-vector
   [unspecified "#110F13" "#b13120" "#719f34" "#ceae3e" "#7c9fc9" "#7868b5" "#009090" "#F4EAD5"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (ujelly)))
 '(custom-safe-themes
   (quote
    ("0f6e58d1814b4138c5a88241f96547d35883cbb3df6cf9ec8ef44856ece04c13"
     "4eaad15465961fd26ef9eef3bee2f630a71d8a4b5b0a588dc851135302f69b16"
     "8cf56691a70156f611ac86d0bbcbc7dee7673df195de5918f34bfdc6814ffd39"
     "62c9339d5cac3a49688abb34e98f87a6ee82003a11251f12e0ada1788090c40f"
     "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6"
     "394504bd559027641b544952d6e9e1c6dcb306b4d1b2c4ad6b98d3e6b5459683"
     "07fe4a500a4812c8fda390e0035aa322f7139d13cf6031bb0fb26ec909cfb67c"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "fe243221e262fe5144e89bb5025e7848cd9fb857ff5b2d8447d115e58fede8f7"
     "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7"
     "f680d237c4c3252876fbfe1561265ff97a5d3ce67fdc5f6c092e33d476ec0993"
     "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f"
     "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4"
     "16e7c7811fd8f1bc45d17af9677ea3bd8e028fce2dd4f6fa5e6535dea07067b1"
     "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8"
     "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad"
     "e890fd7b5137356ef5b88be1350acf94af90d9d6dd5c234978cd59a6b873ea94"
     "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132"
     "1dc3a2e894d5ee9e90035e4ff90d57507857c07e9a394f182a961e935b3b5497"
     "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b"
     "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d"
     "89f613708c8018d71d97e3da7a1e23c8963b798252f1ac2ab813ad63b7a4b341"
     "5bff694d9bd3791807c205d8adf96817ee1e572654f6ddc5e1e58b0488369f9d"
     "787574e2eb71953390ed2fb65c3831849a195fd32dfdd94b8b623c04c7f753f0"
     "d921083fbcd13748dd1eb638f66563d564762606f6ea4389ea9328b6f92723b7"
     "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460"
     "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e"
     "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19"
     "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4"
     "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c"
     "923faef2c7ed017e63f517703c846c6190c31400261e8abdb1be06d5b46ea19a"
     "617219c11282b84761477059b9339da78ce392c974d9308535ee4ec8c0770bee"
     "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163"
     "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa"
     "e065de66980983399a7367f8eec3b32e88c932d77b9b34e9f4a380582855b504"
     default)))
 '(ecb-options-version "2.32")
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#202020")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 6 nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#F2F2F2" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#6DA8D2" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#F2F2F2" . 100))))
 '(jabber-account-list
   (quote
    (("ravi@jabber.locationlabs.com"
      (:network-server . "jabber.locationlabs.com")
      (:port . 5222)
      (:connection-type . starttls))
     ("gorrepati@gmail.com"
      (:network-server . "talk.google.com")
      (:port . 5223)
      (:connection-type . ssl)))))
 '(jabber-alert-message-hooks
   (quote
    (jabber-message-screen jabber-message-echo jabber-message-scroll)))
 '(jabber-alert-presence-hooks nil)
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-default-status "DONT PANIC!")
 '(jabber-display-menu t)
 '(jabber-history-enabled t)
 '(jabber-history-size-limit 1024000)
 '(jabber-muc-autojoin (quote ("cni@conference.jabber.locationlabs.com")))
 '(jabber-roster-default-group-name nil)
 '(jabber-use-global-history t)
 '(jabber-vcard-avatars-retrieve nil)
 '(linum-format "%d")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(nexus-rest-url "http://nexus1/service/local/lucene/search")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(safe-local-variable-values
   (quote
    ((nxml-attribute-indent . 2)
     (nxml-child-indent . 2)
     (nxml-attribute-indent . 3)
     (nxml-child-indent . 3))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smtpmail-smtp-server "webmail.locationlabs.com")
 '(smtpmail-smtp-service 587)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2b2b2b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#bc8383")
     (40 . "#cc9393")
     (60 . "#dfaf8f")
     (80 . "#d0bf8f")
     (100 . "#e0cf9f")
     (120 . "#f0dfaf")
     (140 . "#5f7f5f")
     (160 . "#7f9f7f")
     (180 . "#8fb28f")
     (200 . "#9fc59f")
     (220 . "#afd8af")
     (240 . "#bfebbf")
     (260 . "#93e0e3")
     (280 . "#6ca0a3")
     (300 . "#7cb8bb")
     (320 . "#8cd0d3")
     (340 . "#94bff3")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
