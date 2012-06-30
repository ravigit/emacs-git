(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar prelude-packages
  '(anything
    anything-complete
    anything-match-plugin
    auto-complete 
    auto-indent-mode
    col-highlight
    clojure-mode
    flymake-shell
    jabber
    javadoc-help
    js2-mode
    mo-git-blame
    moinmoin-mode
    paredit
    pastebin
    powershell
    slime slime-repl 
    org org-blog org-email org-magit org-mime org-outlook org-table-comment org2blog
    yasnippet
    yasnippet-bundle
    zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun init-load-paths ()
  "All the load paths for newly installed packages should go here."
  (let ((load-dirs (list
                    "dvc/lisp"
                    "emacs-tiny-tools/lisp/other"
                    "emacs-tiny-tools/lisp/tiny"
                    "git-emacs"
                    "site-lisp"
                    "wl-2.14.0"
                    )))
    (mapcar (lambda(dir) (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs)))

(defun init-load-pkgs ()
  "Activate the installed packages here"
  (let ((local-pkgs (list
;;                     'auto-complete
                     'blank-mode
                     'bm
                     'context
                     'emacs-os
                     'folding
                     'git-emacs
                     'google
                     'grep-buffers
                     'hide-lines
;                     'init-python
                     'jabber-autoloads
                     'javadoc-help
                     'language-styles
                     'moinmoin-mode
                     'misc
                     'js2-mode
                     'nsi-mode
                     'org-install
                     'psvn
                     'revbufs
                     'show-functions
                     'slime
;                     'xcscope
                     'xub-mode
                     'xub-mode
                     'yasnippet
                     )))
    (mapcar 'require local-pkgs)))

(defun init-set-modes ()
  "The state emacs should be in when it starts up, with all
 the minor minor modes and window decorations set here"
  (progn
    (column-number-mode t)
    (desktop-save-mode 1)
    (fringe-mode)
    (global-hi-lock-mode 1)
    (global-linum-mode t)
;;    (global-autocomplete-mode t)
    ;; (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (setq scroll-step 1)
    (show-paren-mode t)
    (size-indication-mode t)
    (tool-bar-mode -1)
    (which-function-mode t)
    (semantic-mode 1)))

(defun init-set-key-mappings ()
  "All the key mappings go here"
  (let ((mappings (list
                   '("\C-\\"     uncomment-line-or-region)
                   '("\C-c/"     comment-line-or-region)
                   '("\C-c0"     bm-show-all)
                   '("\C-c1"     bm-toggle)
                   '("\C-c2"     bm-next)
                   '("\C-c3"     bm-previous)
                   '("\C-c'"     gnus)
                   '("\C-c4"     javadoc-lookup)
                   '("\C-c?"     open-org)
                   '("\C-c\C-b"  search-forward-regexp)
                   '("\C-c\C-t"  restart-tomcat)
                   '("\C-ca"     open-fileline)
                   '("\C-cb"     grep-buffers)
                   '("\C-cc"     compile)
                   '("\C-cd"     svn-file-show-svn-diff)
                   '("\C-ce"     blank-mode)
                   '("\C-cf"     grep)
                   '("\C-cg"     google-it)
                   '("\C-ch"     hide-lines)
                   '("\C-ci"     transpose-window-orientation)
                   '("\C-cj"     cua-mode)
                   '("\C-ck"     tags-apropos)
                   '("\C-cl"     open-finder-sprint-log)
                   '("\C-cn"     auto-revert-tail-mode)
                   '("\C-co"     eval-buffer)
                   '("\C-cp"     goto-line)
                   '("\C-cq"     replace-string)
                   '("\C-crf"    recursive-grep)
                   '("\C-cs"     cscope-find-global-definition)
                   '("\C-cu"     my-browse-url)
                   '("\C-cv"     indent-buffer)
                   '("\C-cw"     open-replay-dir)
                   '("\C-cz"     close-all-buffers)
                   '([f1]        search-forward-regexp)
                   '([f2]        search-backward-regexp)
                   '([f3]        xah-emacs-help))))
    (mapcar (lambda (mapping)
              (let ((key (car mapping))
                    (func (cadr mapping)))
                (progn
                  (message (format "map key %s to %s" key func))
                  (global-set-key key func))))
            mappings)))

(defun init-set-vars ()
  "Default variables that control the behavior of emacs"
  (progn
    (setq-default tab-width 3)
    (setq-default indent-tabs-mode nil)
    (setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
    (setq rcirc-server-alist  '(("bugz" :channels  ("#engr"))))
    (setq scroll-down-aggressively t)))

;; (defun init-yasnippet ()
;;   "Initialize yasnippet"
;;   (progn
;;     (yas/initialize)
;;     (yas/load-directory "~/.emacs.d/yasnippet/snippets")))

(defun init-other ()
  "All the other misc. stuff that doesn't fit any where else goes here"
  (progn
    ;; (init-yasnippet)
    (add-to-list 'auto-mode-alist '("\\.js$" .  js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp$" . java-mode))
    (add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))
    (add-to-list 'auto-mode-alist '("\\.nsh$" . nsi-mode))
    (setq inferior-lisp-program "sbcl")
;    (slime-setup '(slime-fancy slime-asdf))
))

(defun init-hooks ()
  "Hooks for all the modes go here"
  (add-hook 'dired-load-hook (lambda () (load "dired-x")))
  (add-hook 'lisp-mode-hook  (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t))))

(defun init-server ()
  "Just a wrapper function for emacs server"
  (server-start))

(defun init-local ()
  "A wrapper function that loads the local customizations, those that belong to
  a machine."
  ;; (require 'emacs-local)
)


(defun init-all ()
  "The index of all functions that get loaded during initialization"
  (progn
    (init-load-paths)
    (init-load-pkgs)
    (init-set-modes)
    (init-set-key-mappings)
    (init-set-vars)
    (init-hooks)
    (init-other)
    (init-server)
    (init-local)))

(init-all)


; Don't exit without confirmation
(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes (quote ("5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "e065de66980983399a7367f8eec3b32e88c932d77b9b34e9f4a380582855b504" default)))
 '(ecb-options-version "2.32")
 '(jabber-account-list (quote (("ravi@jabber.locationlabs.com" (:network-server . "jabber.locationlabs.com") (:port . 5222) (:connection-type . starttls)) ("gorrepati@gmail.com" (:network-server . "talk.google.com") (:port . 5223) (:connection-type . ssl)))))
 '(jabber-alert-message-hooks (quote (jabber-message-screen jabber-message-echo jabber-message-scroll)))
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
 '(safe-local-variable-values (quote ((nxml-attribute-indent . 2) (nxml-child-indent . 2) (nxml-attribute-indent . 3) (nxml-child-indent . 3))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smtpmail-smtp-server "mail.locationlabs.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 103 :width normal))))
 '(jabber-roster-user-away ((t (:foreground "dark green" :weight normal)))))
