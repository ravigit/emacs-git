
(defun init-load-paths ()
  "All the load paths for newly installed packages should go here."
  (let ((load-dirs (list
                    "color-theme-6.6.0"
                    "dvc/lisp"
                    "emacs-tiny-tools/lisp/other"
                    "emacs-tiny-tools/lisp/tiny"
                    "git-emacs"
                    "org-7.7/contrib/lisp"
                    "org-7.7/lisp"
                    "powershell"
                    "site-lisp"
                    "wl-2.14.0"
                    "yasnippet"
                    "slime"
                    "emacs-jabber"
                    )))
    (mapcar (lambda(dir) (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs)))

(defun init-load-pkgs ()
  "Activate the installed packages here"
  (let ((local-pkgs (list
                     'blank-mode
                     'blank-mode
                     'bm
                     'context
                     'emacs-os
                     'folding
                     'git-emacs
                     'google
                     'grep-buffers
                     'hide-lines
                     'language-styles
                     'locationlabs
                     'misc
                     'nsi-mode
                     'org-install
                     'psvn
                     'revbufs
                     'show-functions
                     'xub-mode
                     'xub-mode
                     'yasnippet
                     'slime
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
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (setq scroll-step 1)
    (show-paren-mode t)
    (size-indication-mode t)
    (tool-bar-mode -1)
    (which-function-mode t)))

(defun init-set-key-mappings ()
  "All the key mappings go here"
  (let ((mappings (list
                   ;; '("\C-/ "     comment-line-or-region)
                   '("\C-\\"     uncomment-line-or-region)
                   '("\C-c/"     comment-line-or-region)
                   '("\C-c0"     bm-show-all)
                   '("\C-c1"     bm-my-toggle)
                   '("\C-c2"     bm-next)
                   '("\C-c3"     bm-previous)
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
                   '("\C-ci"     open-index)
                   '("\C-ck"     tags-apropos)
                   '("\C-cl"     open-finder-sprint-log)
                   '("\C-cn"     auto-revert-tail-mode)
                   '("\C-co"     eval-buffer)
                   '("\C-cp"     goto-line)
                   '("\C-cq"     replace-string)
                   '("\C-crf"    recursive-grep)
                   '("\C-cs"     cua-mode)
                   '("\C-cu"     my-browse-url)
                   '("\C-cv"     indent-buffer)
                   '("\C-cw"     open-replay-dir)
                   '("\C-cz"     close-all-buffers)
                   '([f1]        search-forward-regexp)
                   '([f2]        search-backward-regexp)
                   '([f3]        xah-emacs-help)
                   '([f5]        search-index)
                   '([f6]        ack)
                   '([f7]        src-update)
                   '([f8]        ll-build))))
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
    (setq scroll-down-aggressively t)
    (setq inferior-lisp-program "/usr/local/bin/sbcl")))

(defun init-yasnippet ()
  "Initialize yasnippet"
  (progn
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/yasnippet/snippets")))

(defun init-other ()
  "All the other misc. stuff that doesn't fit any where else goes here"
  (progn
    (init-yasnippet)
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode))
    (add-to-list 'auto-mode-alist '("\\.nsh$" . nsi-mode))))


(defun init-hooks ()
  "Hooks for all the modes go here"
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x")))
  (add-hook 'lisp-mode-hook 
            (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook 
            (lambda () (inferior-slime-mode t))))

(defun init-server ()
  "Just a wrapper function for emacs server"
  (server-start))


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
    (init-server)))


(init-all)

                                        ; Emacs custom settings(Made by emacs). Proceed with caution
(custom-set-variables
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])

 '(custom-enabled-themes (quote (whiteboard)))
 '(custom-safe-themes (quote ("e065de66980983399a7367f8eec3b32e88c932d77b9b34e9f4a380582855b504" default)))
 '(ecb-options-version "2.32")
 '(safe-local-variable-values (quote ((nxml-attribute-indent . 3) (nxml-child-indent . 3)))))


                                        ; Don't exit without confirmation
(defun confirm-exit-emacs ()
  "ask for confirmation before exiting emacs"
  (interactive)
  (if (yes-or-no-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

(slime-setup '(slime-fancy slime-asdf))
