
;; Set the following to true (t) when moving config to a new machine
(defvar reload-packages nil)

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

(when reload-packages
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
(add-to-list 'exec-path "/usr/bin")
(let ((load-dirs '("site-lisp")))
  (mapc (lambda(dir)
            (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Required Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar local-pkgs
  (list 'emacs-os 'google 'grep-buffers 'hide-lines 'language-styles 'misc 'revbufs 'show-functions 'airline-themes))

(mapc 'require local-pkgs)

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
(add-hook 'clojure-mode-hook (lambda()
                               (yas-minor-mode 1)))
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
  (mapc (lambda (mapping)
          (let ((key (car mapping))
                (func (cadr mapping)))
            (progn
              (message (format "map key %s to %s" key func))
              (global-set-key key func))))
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
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


(global-set-key (kbd "s-.") 'ripgrep-regexp)
(global-set-key (kbd "s-a") 'package-list-packages)
(global-set-key (kbd "s-c") 'cua-mode)
(global-set-key (kbd "s-b") 'indent-buffer)
(global-set-key (kbd "s-p") 'paredit-mode)
(global-set-key (kbd "s-r") 'replace-regexp)
(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "s-l") 'load-theme)
(global-set-key (kbd "s-m") 'magit-status)
(global-set-key (kbd "s-t") 'toggle-window-split)
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
  (cl-loop for theme in custom-enabled-themes
           when (not (string-match "airline" (symbol-name theme)))
           do
           (disable-theme theme)))

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

(add-to-list 'default-frame-alist
             '(font . "Monaco 16"))


;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; ;; Replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


(unless window-system
  (define-key input-decode-map "^[6~" [next])
  (define-key input-decode-map "^[5~" [prior]))


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
   [default default default italic underline success warning error])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(ansi-color-names-vector
   ["#404B5C" "#B26BB8" "#76A8A0" "#C79474" "#6886A6" "#515275" "#7D8AA8" "#8C92A1"])
 '(ansi-term-color-vector
   [unspecified "#0b1c2c" "#bf8b56" "#56bf8b" "#8bbf56" "#8b56bf" "#bf568b" "#8b56bf" "#cbd6e2"] t)
 '(beacon-color "#ec4780")
 '(bm-recenter t)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "9deeab438d1d798c26d41c759d74a2406802427ff6acb7dec8cec961bcb4e7d5" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "d507c9e58cb0eb8508e15c8fedc2d4e0b119123fab0546c5fd30cadd3705ac86" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
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
\"#######..#\" };")) t)
 '(evil-emacs-state-cursor (quote ("#E57373" hbar)) t)
 '(evil-insert-state-cursor (quote ("#E57373" bar)) t)
 '(evil-normal-state-cursor (quote ("#FFEE58" box)) t)
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)) t)
 '(fci-rule-color "#f6f0e1")
 '(fringe-mode 6 nil (fringe))
 '(gnus-logo-colors (quote ("#259ea2" "#adadad")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
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
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#efebe9")
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(linum-format (quote dynamic))
 '(magit-diff-use-overlays nil)
 '(mode-line-format nil)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (java-imports java-snippets monotropic-theme cider autothemer commentary-theme creamsody-theme doneburn-theme doom-themes dracula-theme paredit omtose-phellack-theme cider-decompile cider-hydra ripgrep terraform-mode apropospriate-theme base16-theme basic-theme faff-theme flatui-theme green-phosphor-theme greymatters-theme gruvbox-theme inkpot-theme intellij-theme iodine-theme jazz-theme jbeans-theme lavender-theme lenlen-theme leuven-theme light-soap-theme lush-theme majapahit-theme material-theme metalheart-theme minimal-theme moe-theme oceanic-theme org-beautify-theme paper-theme peacock-theme professional-theme align-cljlet applescript-mode zone-matrix zencoding-mode zenburn zen-mode zen-and-art-theme yoshi-theme yaml-mode xkcd window-margin waher-theme w3 virtualenvwrapper underwater-theme ujelly-theme ubuntu-theme transpose-frame toxi-theme tommyh-theme tango-2-theme sx sublime-themes subatomic-theme steady-theme srefactor spike-theme soothe-theme solarized-theme slime-theme sea-before-storm-theme save-packages remember-theme rainbow-mode rainbow-delimiters qsimpleq-theme pyvenv python-pylint python-pep8 python-mode python-info python-environment pytest pysmell pylint pyimpsort pyflakes pydoc-info pydoc pyde py-autopep8 pep8 pastels-on-dark-theme pastelmac-theme pastebin pandoc-mode paganini-theme org2blog org-table-comment org-mime org-magit org-blog nzenburn-theme nose-mode nlinum nexus moinmoin-mode mo-git-blame minimap magit-tramp magit-topgit magit-gitflow magit-gerrit magit-find-file magit-filenotify magit-annex liso-theme late-night-theme labburn-theme jujube-theme javadoc-lookup jabber ir-black-theme ipython inf-clojure idea-darkula-theme hydandata-light-theme highlight-indentation hide-lines heroku-theme helm-pydoc helm-ls-git helm-helm-commands helm-google helm-git-grep helm-git-files helm-git helm-dash hc-zenburn-theme hamburg-theme gruber-darker-theme groovy-mode gratuitous-dark-theme grandshell-theme gotham-theme gitty github-theme gitconfig-mode gitconfig gandalf-theme forest-blue-theme foggy-night-theme flymake-shell flymake-python-pyflakes flycheck-pos-tip flycheck-clojure flatland-theme flatland-black-theme firecode-theme fill-column-indicator farmhouse-theme eyuml exec-path-from-shell espresso-theme ein-mumamo django-theme distinguished-theme deep-thought-theme dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dark-krystal-theme darcula-theme cyberpunk-theme column-marker colonoscopy-theme col-highlight clues-theme clojure-snippets clojure-quick-repls clojure-mode-extra-font-locking clojure-cheatsheet cherry-blossom-theme caroline-theme calmer-forest-theme butler busybee-theme bubbleberry-theme boron-theme borland-blue-theme bm bliss-theme blank-mode birds-of-paradise-plus-theme bash-completion badwolf-theme badger-theme autopair auto-yasnippet auto-shell-command auto-save-buffers-enhanced auto-install auto-indent-mode auto-highlight-symbol auto-dictionary auto-complete-sage auto-complete-rst auto-complete-pcmp auto-complete-nxml auto-complete-exuberant-ctags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers atom-one-dark-theme atom-dark-theme assemblage-theme arjen-grey-theme apt-utils ample-zen-theme ample-theme alect-themes airline-themes ahungry-theme afternoon-theme adaptive-wrap ack ac-python ac-nrepl ac-js2 ac-ispell ac-cider abyss-theme 4clojure)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(scroll-bar-mode nil)
 '(semantic-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tabbar-background-color "#353535")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#d81212")
     (40 . "#f71010")
     (60 . "#6a621b")
     (80 . "#958323")
     (100 . "#ef8300")
     (120 . "#9ca30b")
     (140 . "#008b45")
     (160 . "#077707")
     (180 . "#028902")
     (200 . "#1c9e28")
     (220 . "#3cb368")
     (240 . "#0d7b72")
     (260 . "#358d8d")
     (280 . "#286060")
     (300 . "#0505cc")
     (320 . "#0000ff")
     (340 . "#2c53ca")
     (360 . "#a020f0"))))
 '(vc-annotate-very-old-color "#a020f0")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))) t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(zen-encumbered-urls
   (quote
    ("#brief timewastes" "www.penny-arcade.com" "www.dilbert.com" "www.xkcd.com" "www.userfriendly.org" "#news waste" "slashdot.org" "dn.se" "#social timewastes" "https://www.facebook.com")))
 '(zen-fullscreen-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bm-face ((t (:underline t))))
 '(bm-fringe-face ((t (:underline t)))))
