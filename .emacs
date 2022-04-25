;; Sorry for this stuff at the top. Don't see a easy way.
;; Go past this for the actual config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(ansi-color-names-vector
   ["#404B5C" "#B26BB8" "#76A8A0" "#C79474" "#6886A6" "#515275" "#7D8AA8" "#8C92A1"])
 '(ansi-term-color-vector
   [unspecified "#0b1c2c" "#bf8b56" "#56bf8b" "#8bbf56" "#8b56bf" "#bf568b" "#8b56bf" "#cbd6e2"] t)
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(beacon-color "#ec4780")
 '(bm-recenter t)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("ff3d075ecd4c9e46efb8effecbe24116085978d3e7f879739d0c4539a49cc03e" "d0e166972a0a8cdd4e91e4b5a040ef429c733f1bf4f4117351bf12b7a8967a59" "cd322dc37af17c4e122c99c93fe1a423dd1407797fe51d2278fc25c60a46be45" "2ed177de0dfc32a6a32d6109ddfd1782a61bcc23916b7b967fa212666d1aa95c" "0cd00c17f9c1f408343ac77237efca1e4e335b84406e05221126a6ee7da28971" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "9deeab438d1d798c26d41c759d74a2406802427ff6acb7dec8cec961bcb4e7d5" "6a23db7bccf6288fd7c80475dc35804c73f9c9769ad527306d2e0eada1f8b466" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" "d507c9e58cb0eb8508e15c8fedc2d4e0b119123fab0546c5fd30cadd3705ac86" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"#######..#\" };") t)
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#f6f0e1")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(fringe-mode 6 nil (fringe))
 '(gnus-logo-colors '("#259ea2" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
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
\"###########.######\" };") t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("HOLD" . "#cfdf30")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae8fc")
     ("DONT" . "#70c900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#f0ce43")
     ("KLUDGE" . "#eecc00")
     ("HACK" . "#eecc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9977")
     ("XXX+" . "#f4923b")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'diredp-deletion-file-name)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'diredp-flag-mark)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(linum-format 'dynamic)
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style 'chamfer)
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(flycheck-clj-kondo clj-refactor json-navigator json-mode java-imports java-snippets monotropic-theme cider autothemer commentary-theme creamsody-theme doneburn-theme doom-themes dracula-theme paredit omtose-phellack-theme cider-decompile cider-hydra ripgrep terraform-mode apropospriate-theme base16-theme basic-theme faff-theme flatui-theme green-phosphor-theme greymatters-theme gruvbox-theme inkpot-theme intellij-theme iodine-theme jazz-theme jbeans-theme lavender-theme lenlen-theme leuven-theme light-soap-theme lush-theme majapahit-theme material-theme metalheart-theme minimal-theme moe-theme oceanic-theme org-beautify-theme paper-theme peacock-theme professional-theme align-cljlet applescript-mode zone-matrix zencoding-mode zenburn zen-mode zen-and-art-theme yoshi-theme yaml-mode xkcd window-margin waher-theme w3 virtualenvwrapper underwater-theme ujelly-theme ubuntu-theme transpose-frame toxi-theme tommyh-theme tango-2-theme sx sublime-themes subatomic-theme steady-theme srefactor spike-theme soothe-theme solarized-theme slime-theme sea-before-storm-theme save-packages remember-theme rainbow-mode rainbow-delimiters qsimpleq-theme pyvenv python-pylint python-pep8 python-mode python-info python-environment pytest pysmell pylint pyimpsort pyflakes pydoc-info pydoc pyde py-autopep8 pep8 pastels-on-dark-theme pastelmac-theme pastebin pandoc-mode paganini-theme org2blog org-table-comment org-mime org-magit org-blog nzenburn-theme nose-mode nlinum nexus moinmoin-mode mo-git-blame minimap magit-tramp magit-topgit magit-gitflow magit-gerrit magit-find-file magit-filenotify magit-annex liso-theme late-night-theme labburn-theme jujube-theme javadoc-lookup jabber ir-black-theme ipython inf-clojure idea-darkula-theme hydandata-light-theme highlight-indentation hide-lines heroku-theme helm-pydoc helm-ls-git helm-helm-commands helm-google helm-git-grep helm-git-files helm-git helm-dash hc-zenburn-theme hamburg-theme gruber-darker-theme groovy-mode gratuitous-dark-theme grandshell-theme gotham-theme gitty github-theme gitconfig-mode gitconfig gandalf-theme forest-blue-theme foggy-night-theme flymake-shell flymake-python-pyflakes flycheck-pos-tip flycheck-clojure flatland-theme flatland-black-theme firecode-theme fill-column-indicator farmhouse-theme eyuml exec-path-from-shell espresso-theme ein-mumamo django-theme distinguished-theme deep-thought-theme dash-at-point darktooth-theme darkokai-theme darkmine-theme darkburn-theme dark-krystal-theme darcula-theme cyberpunk-theme column-marker colonoscopy-theme col-highlight clues-theme clojure-snippets clojure-quick-repls clojure-mode-extra-font-locking clojure-cheatsheet cherry-blossom-theme caroline-theme calmer-forest-theme butler busybee-theme bubbleberry-theme boron-theme borland-blue-theme bm bliss-theme blank-mode birds-of-paradise-plus-theme bash-completion badwolf-theme badger-theme auto-yasnippet auto-shell-command auto-save-buffers-enhanced auto-install auto-indent-mode auto-highlight-symbol auto-dictionary auto-complete-sage auto-complete-rst auto-complete-pcmp auto-complete-nxml auto-complete-exuberant-ctags auto-complete-clang-async auto-complete-clang auto-complete-chunk auto-complete-c-headers atom-one-dark-theme atom-dark-theme assemblage-theme arjen-grey-theme apt-utils ample-zen-theme ample-theme alect-themes airline-themes ahungry-theme afternoon-theme adaptive-wrap ack ac-python ac-nrepl ac-js2 ac-ispell ac-cider abyss-theme 4clojure))
 '(pdf-view-midnight-colors '("#ffffff" . "#110b11"))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(scroll-bar-mode nil)
 '(semantic-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tabbar-background-color "#353535")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#d81212")
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
     (360 . "#a020f0")))
 '(vc-annotate-very-old-color "#a020f0")
 '(warning-suppress-log-types '((comp)))
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(when
      (or
       (not
        (boundp 'ansi-term-color-vector))
       (not
        (facep
         (aref ansi-term-color-vector 0)))) t)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(zen-encumbered-urls
   '("#brief timewastes" "www.penny-arcade.com" "www.dilbert.com" "www.xkcd.com" "www.userfriendly.org" "#news waste" "slashdot.org" "dn.se" "#social timewastes" "https://www.facebook.com"))
 '(zen-fullscreen-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "IBM Plex Mono"))))
 '(bm-face ((t (:underline t))))
 '(bm-fringe-face ((t (:underline t)))))

(let ((load-dirs '("site-lisp")))
  (mapc (lambda(dir)
            (add-to-list 'load-path (concat "~/.emacs.d/" dir))) load-dirs))


;; Set the following to true (t) when moving config to a new machine
(defvar reload-packages t)


;; ALL EMACS CONFIGURATION IS IN emacs-mine CONFIG FILE. PLEASE MAKE CHANGES TO THAT FILE, NOT THE CURRENT ONE
;; IT IS LOCATED AT ~/.emacs.d/site-lisp/emacs-mine.el

(require 'emacs-mine)

;; END OF CONFIG FILE
