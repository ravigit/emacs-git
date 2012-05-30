;; M-x bscope-init
;; M-x bscope-find-global-definition

;; comment:

;; Because xcscope.el open a new process of "cscope -L" for each a
;; definition, it's wasting time for reloading cscope and reopening
;; database. It's so slow that I can't bear. So I wrote a simple
;; lisp script using "cscope -l" instead. It saved a lot of time
;; without reloading cscope.

;; Thanks to xcscope.el and Darryl Okahata, I learn a lot from them.

;; There may be a lot of bugs, and no warranty for this script

;; Joy it.
;; Feel free to hack yourself. Just send me your copy.

;; If you find any bugs, please let me know.
;; I don't assure reply for each.

;; My email address is
;; highfly22 at gmail dot com



(defun bscope-canonicalize-directory (dir)
  (or dir
      (setq dir default-directory))
  (setq dir (file-name-as-directory
             (expand-file-name (substitute-in-file-name dir))))
  dir
  )


;; (setq default-directory "g:/software/cscope-15.4/src/")



; process name "bscope"
; buffer name "*bscope*"
; program name "cscope"
; option "-l"
;; (start-process "bscope" "*bscope*" "cscope" "-l")

;; (process-send-string "bscope" "1main\n")

;; (process-send-string "bscope" "0yylex\n")

;; (process-send-string "bscope" "\n")

(defvar bscope-marker-ring-length 16 )

(defvar bscope-marker-ring (make-ring bscope-marker-ring-length)
  )



(defun bscope-init (dir)
  (interactive "DCscope Initial Directory: ")
  ;;   (interactive)
  (setq default-directory  dir)
  (start-process "bscope" "*bscope*" "cscope" "-l")
  ;;   (set-process-sentinel (get-process "bscope") 'bscope-msg-me)
  (set-process-filter (get-process "bscope") 'bscope-filter)
  (with-current-buffer "*bscope*"
    (accept-process-output (get-process "bscope") 10)
    (if (looking-at "^cscope: no source files found")
        (progn
          (erase-buffer)
          (message "bscope: no scope.out file here"))
      (progn
        (bscope-wait-for-output)
        (message  "bscope: load ok")
        )
      )
    )
  )

;; (defun bscope-msg-me (process event)
;;   (message
;;    (format "Process: %s had the event %s" process event)))



(defun bscope-filter (process string)
  ;; Write the output into the Tramp Process
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (point-max))
      (insert string)
      ))
  )

(defun bscope-query (command)
  (let ((proc (get-process "bscope"))
        )
    (with-current-buffer (process-buffer proc)

      (goto-char (point-max))
      (insert command)

      (process-send-string "bscope" command)

      (bscope-wait-for-output )

      (bscope-process-output )
      )
    )
  )

;; (bscope-query "0yylex\n")
;; (bscope-query "0em_receive\n")


(defun bscope-find-global-definition (symbol)
  (interactive (bscope-interactive "Find this global definition: "))
  (setq query-command  (concat "1" symbol "\n") )
  (ring-insert bscope-marker-ring (point-marker))
  (bscope-query query-command)
  )

(defun bscope-interactive (prompt)
  (list
   (let (sym)
     (setq sym (current-word))
     (read-string
      (if sym
          (format "%s (default %s): "
                  (substring prompt 0 (string-match "[ :]+\\'" prompt))
                  sym)
        prompt)
      nil nil sym)
     ))
  )

(defun bscope-process-output ()

  (with-current-buffer "*bscope*"
    (setq stuff (buffer-substring-no-properties (point) (point-max)))
    (while (and stuff
                (string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)" stuff))
      (setq line (substring stuff
                            (match-beginning 1) (match-end 1)))
      (setq stuff (substring stuff
                             (match-beginning 2)
                             (match-end 2)))
      (if (= (length stuff) 0)
          (setq stuff nil))


      (if (string-match
           "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[
\t]+\\(.*\\)\n"
           line)
          (progn
            (let (str)
              (setq file (substring line (match-beginning 1)
                                    (match-end 1))
                    function-name (substring line (match-beginning 2)
                                             (match-end 2))
                    line-number (string-to-number (substring line 
(match-beginning 3)
                                                             (match-end 3)) )
                    line (substring line (match-beginning 4)
                                    (match-end 4))
                    )
              ;; move to new file

              (switch-to-buffer (find-file file))
              (if (> line-number 0)
                  (progn
                    (goto-line line-number)

                    ))
              )
            )

        (progn
          (message "bscope: Sorry, on found")
          )
        ))))


(defun bscope-pop-mark()
  (interactive)
  (if (ring-empty-p bscope-marker-ring)
      (error "No previous locations for bscope invocation"))
  (let ((marker (ring-remove bscope-marker-ring 0)))
    (switch-to-buffer (or (marker-buffer marker)
                          (error "The marked buffer has been
deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))


(defun bscope-wait-for-output (&optional timeout)

  (let ((proc (get-buffer-process (current-buffer)))
        (found nil)
        (start-time (current-time))
        (start-point (point)))

    (save-excursion
      (while (not found)
        (accept-process-output proc 1)
        (goto-char (point-max))         ;move the last line
        (beginning-of-line)          ;move the beginning of last line
        (setq found (looking-at "^>>"))) ;looking for cscope prompt "^>>"
      )
    )
  )



(provide 'bscope)