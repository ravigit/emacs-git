(defmacro set-global-key (key func)
  (global-set-key (kbd key) func))

(set-global-key "C-c z" 'eval-buffer)
