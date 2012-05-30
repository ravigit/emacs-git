(deftheme ravi
  "Created 2012-03-01.")

(custom-theme-set-variables
 'ravi
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(git-baseline-alist (quote (("/ext/home/ravi/git_project/scripts/replicateDb/" . "master"))))
 '(safe-local-variable-values (quote ((sgml-indent-level . 2) (nxml-attribute-indent . 2) (nxml-child-indent . 2) (nxml-attribute-indent . 3) (nxml-child-indent . 3))))
 '(tool-bar-mode nil)
 '(show-paren-mode t)
 '(display-time-mode t)
 '(column-number-mode t)
 '(cua-mode t))

(custom-theme-set-faces
 'ravi
 '(default ((t (:family "FreeMono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal)))))

(provide-theme 'ravi)
