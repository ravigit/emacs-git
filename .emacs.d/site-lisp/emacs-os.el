(cond ((eq system-type 'gnu/linux) (load ".emacs.linux"))
      ((eq system-type 'darwin) (load ".emacs.darwin"))
      ((eq system-type 'windows-nt) (load ".emacs.windows"))) 


(provide 'emacs-os)
