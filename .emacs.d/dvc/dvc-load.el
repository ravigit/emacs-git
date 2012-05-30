; -*- mode: emacs-lisp -*-
;;
;; Load DVC easily ...
;;
;; Manually, you can run
;;
;;   M-x load-file RET /path/to/dvc-load.el RET
;;
;; (usefull when you want to load DVC after starting "emacs -q"!), or
;; add
;;
;;   (load-file "/path/to/this/file/in/builddir/dvc-load.el")
;;
;; to your ~/.emacs.el

(add-to-list 'load-path "/cygdrive/c/Documents and Settings/QA_Testing/.my_emacs/emacs/.emacs.d/dvc/lisp")
(unless (locate-library "ewoc")
  (add-to-list 'load-path "/cygdrive/c/Documents and Settings/QA_Testing/.my_emacs/emacs/.emacs.d/dvc/lisp/contrib"))
(add-to-list 'Info-default-directory-list "/cygdrive/c/Documents and Settings/QA_Testing/.my_emacs/emacs/.emacs.d/dvc/texinfo")

(if (featurep 'dvc-core)
    (dvc-reload)
  (if (featurep 'xemacs)
      (require 'dvc-autoloads "/cygdrive/c/Documents and Settings/QA_Testing/.my_emacs/emacs/.emacs.d/dvc/lisp/auto-autoloads.elc")
    (require 'dvc-autoloads)))

