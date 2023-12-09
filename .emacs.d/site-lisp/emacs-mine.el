;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THIS IS THE MAIN CONFIG FILE. THIS IS WHERE YOU WOULD CONFIGURE ;;
;; EMACS							                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)


(setq package-archives '(                        
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
			                ("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(when reload-packages
  (if (< emacs-major-version 25)
    (progn
      (package-refresh-contents)
      (when (not (package-installed-p 'save-packages))
	(package-install 'save-packages))
      ;; Restore all the saved packages from the index.
      ;; The following poses an interactive question, wether or not
      ;; to install a package. To say yes for all, choose the option '!'
      (install-saved-packages))
    ;else
    (package-install-selected-packages)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Set and LoadPaths  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/bin")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Required Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ansi-color)

(setq comp-deferred-compilation-deny-list '("powerline"))

(defvar local-pkgs
  (list 'emacs-os 'google 'grep-buffers 'hide-lines 'language-styles  'revbufs 'show-functions 'epigrams
	     ;;'misc
	     'airline-themes
	))

(mapc 'require local-pkgs)
(require 'lsp-mode)

;; (require 'airline-themes)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set mode parameters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode)
(column-number-mode t)
(desktop-save-mode 1)
(fringe-mode)
(global-hi-lock-mode 1)
(global-nlinum-mode t)
(scroll-bar-mode -1)
(semantic-mode 1)
(setq scroll-step 1)
(show-paren-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
;(which-function-mode t) ;; Enabling this seems to hang emacs on startup


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
(setq cider-repl-display-help-banner nil)
(setq  cljr-suppress-middleware-warnings t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add mode bindings to file types ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.gradle$" .     groovy-mode))
(add-to-list 'auto-mode-alist '("\\.js$" .         js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" .        java-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" .   markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" .         markdown-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" .        clojure-mode))


(eval-after-load 'flycheck '(flycheck-clojure-setup))
;; (add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;;;;;;;;;;;;;;;
;; Add hooks ;;
;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'org-mode-hook (lambda () (flyspell-mode)))
(add-hook 'clojure-mode-hook (lambda() (enable-paredit-mode)))
(add-hook 'clojure-mode-hook (lambda() (rainbow-delimiters-mode)))
(add-hook 'clojure-mode-hook (lambda() (yas-minor-mode 1)))
(add-hook 'markdown-mode-hook (lambda() (flyspell-mode 1)))
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(add-hook 'java-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook (lambda() (highlight-indentation-mode)))

(defun run-java-file ()
  (interactive)
  (let* ((cb         (current-buffer))
         (buf-name   (concat "run-java-" (buffer-name cb)))
         (file       (file-name-nondirectory (buffer-file-name cb)))
         (class-file (replace-regexp-in-string "\\.java" "" file)))
    (switch-to-buffer-other-window buf-name)
    (goto-char (point-max))
    (insert (format-time-string "\n%D %-I:%M %p: (Re)Compiling and Running\n"))
    (call-process "javac" nil buf-name t file)
    (call-process "java" nil buf-name t class-file)))


(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

 (defun date (arg)
   (interactive "P")
   (insert (if arg
               (format-time-string "%d.%m.%Y")
             (format-time-string "%%m-%d-%Y"))))

(defun timestamp ()
   (interactive)
   (insert (format-time-string "%%m-%d-%Y %H:%M:%S")))


;;;;;;;;;;;;;;;
;; Key maps  ;;
;;;;;;;;;;;;;;;
(let ((mappings (list
                 '("C-\\"		uncomment-line-or-region)
					  '("C-c '"		gnus)
					  '("C-c ."		add-to-imports)
					  '("C-c /"		comment-line-or-region)
					  '("C-c 0"		bm-show-all)
					  '("C-c 1"		bm-toggle)
					  '("C-c 2"		bm-next)
					  '("C-c 3"		bm-previous)
					  '("C-c ;"		search-index)
					  '("C-c ?"		open-org)
					  '("C-c C-b"	search-forward-regexp)
					  '("C-c C-t"	restart-tomcat)
					  '("C-c a"		open-fileline)
					  '("C-c b"		grep-buffers)
					  '("C-c c"		compile)
					  '("C-c d"		vc-diff)
					  '("C-c e"		blank-mode)
					  '("C-c f"		grep)
					  '("C-c g"		google-it)
					  '("C-c h"		hide-lines)
					  '("C-c i"		transpose-frame)
					  '("C-c j"		join-line)
					  '("C-c k"		tags-apropos)
					  '("C-c l"		cider-connect) ; clojure uses cider
					  '("C-c n"		auto-revert-tail-mode)
					  '("C-c o"		eval-buffer)
					  '("C-c p"		goto-line)
					  '("C-c q"		query-replace)
					  '("C-c r f"		recursive-grep)
					  '("C-c s"		cscope-find-global-definition)
					  '("C-c u"		my-browse-url)
					  '("C-c v"		indent-buffer)
					  '("C-c y"		duplicate-line)
					  '("C-c x"		replace-string)
					  '("C-c z"		close-all-buffers)
					  '("M-["		beginning-of-defun)
					  '("M-]"		end-of-defun)
                 ;; Don't know how to get these working in emacs29
                 ;; '([f1]        search-forward-regexp)
                 ;; '([f2]        search-backward-regexp)
                 ;; '([f3]        xah-emacs-help)
                 ;; '([f5]        search-index)
                 )))
  (mapc (lambda (mapping)
          (let ((key (car mapping))
                (func (cadr mapping)))
            (progn
              (message (format "map key %s to %s" key func))
              (keymap-global-set key func))))
        mappings))

(defun disable-themes ()
  (interactive)
  "Disable all loaded themes"
  (mapc 'disable-theme custom-enabled-themes))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-funcall)))
	  (window splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


(defun kill-all-cider-buffers ()
  "Kill all cider buffers."
  (interactive)
  (cl-loop for buffer in (buffer-list)
         when (string-match "cider-repl" (buffer-name buffer))
         do
         (kill-buffer buffer)))


(keymap-global-set "s-." 'ripgrep-regexp)
(keymap-global-set "s-a" 'package-list-packages)
(keymap-global-set "s-c" 'cua-mode)
(keymap-global-set "s-b" 'indent-buffer)
(keymap-global-set "s-p" 'paredit-mode)
(keymap-global-set "s-r" 'replace-regexp)
(keymap-global-set "s-s" 'replace-string)
(keymap-global-set "s-l" 'load-theme)
(keymap-global-set "s-m" 'magit-status)
(keymap-global-set "s-t" 'toggle-window-split)
(keymap-global-set "s-v" 'magit-diff)
(keymap-global-set "s-n" 'magit-blame)
(keymap-global-set "s-/" 'comment-line-or-region)
(keymap-global-set "s-\\" 'uncomment-line-or-region)
(keymap-global-set "s-d" 'disable-themes)
(keymap-global-set "s-w" 'whack-whitespace)
(keymap-global-set "s-1" 'xterm-color-colorize-buffer)
(keymap-global-set "s-3" 'kill-all-cider-buffers)


;; (keymap-set clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

(defadvice show-paren-function (after my-echo-paren-matching-line activate)
  "If a matching paren is off-screen, echo the matching line."
  (when (char-equal (char-syntax (char-before (point))) ?\))
    (let ((matching-text (blink-matching-open) ))
      (when matching-text
        (message matching-text)))))

(defadvice load-theme (before disable-before-load)
  "Disable any loaded themes before enabling a new theme.
 This prevents overlapping themes; something I would rarely want."
  (cl-loop for theme in custom-enabled-themes
           when (not (string-match "airline" (symbol-name theme)))
           do
           (disable-theme theme)))

(ad-activate 'load-theme)

;(load-theme 'airline-cool)
;(load-theme 'atom-one-dark)

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
        (save-buffers-kill-emacs))))

(keymap-global-unset "C-x C-c")
(keymap-global-set "C-x C-c" 'confirm-exit-emacs)


;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))


(unless window-system
  (keymap-set input-decode-map "^[6~" [next])
  (keymap-set input-decode-map "^[5~" [prior]))

(defun helm-insert-char ()
  (interactive)
  (helm :sources
      `((name . "Unicode char name")
        (candidates . ,(ucs-names))
        (action . insert))))


(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
         alpha
       (cdr alpha)) ; may also be nil
     100)
    (set-frame-parameter nil 'alpha '(85 . 50))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

;;;;;;;;;;;;;;;;;;
;; Emacs Server ;;
;;;;;;;;;;;;;;;;;;

(server-start)


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "zephyr:latest" :embedding-model "zephyr:latest")))


(provide 'emacs-mine)
