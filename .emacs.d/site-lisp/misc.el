(require 'url)

(setq xah-doc-location "C:/Docume~1/QA_Testing/Desktop/xah_emacs_tutorial/")

(defun my-browse-url (url)
  (interactive "sBrowse: ")
  (browse-url (concat "http://" url)))

(defun gnus-mail ()
  (interactive)
  (let ((inbox-line -2)
        (mails-to-show 30))
    (progn
      (gnus)
      (forward-line inbox-line)
      (gnus-group-read-group mails-to-show t)
      (forward-line (- mails-to-show 2)))))

;; There is a better way to do this. Set ssh-askpass-gnome
;; (defun sudo-shell-command (command) 
;;   (interactive "scommand:")
;;   (shell-command (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Commenting code from http://www.tenfoot.org.uk/emacs/snippets.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar comment-insert-block-on-empty-line nil
  "Whether to insert block comments on empty lines in comment-insert")

(defun comment-insert ()
  "Insert a new comment on current line"
  (interactive)
  (let ((empty-line (save-excursion (beginning-of-line) (looking-at "\\s-*$"))))
    (if (and (not (equal (point) (point-at-bol)))
             (save-excursion (backward-char) (looking-at "\\S-")))
        ;; insert space if immediately preceding char not whitespace
        (insert " "))
    (insert comment-start)
    (if (and empty-line comment-insert-block-on-empty-line)
        (comment-indent-new-line))
    (save-excursion
      (if (and empty-line comment-insert-block-on-empty-line)
          (insert-and-inherit ?\n))
      (insert comment-end)
      (indent-for-tab-command))
    (indent-for-tab-command)))


(defun comment-line-or-region (&optional comment-eol)
  "comment the region if active otherwise comment the current line"
  (interactive "P")
  (if mark-active
      ;; comment selection
      (comment-region (point) (mark))
    (if (or comment-eol
            (save-excursion (beginning-of-line) (looking-at "\\s-*$")))
        ;; insert comment at end of line
        (comment-insert)
      ;; comment whole line
      (comment-region (point-at-bol) (point-at-eol)))))

(defun uncomment-line-or-region ()
  "uncomment the region if active otherwise comment the current line"
  (interactive)
  (if mark-active
      (uncomment-region (point) (mark))
    (uncomment-region (point-at-bol) (point-at-eol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; full screen. Only works on linux ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-fullscreen (&optional f)
      (interactive)
      (let ((current-value (frame-parameter nil 'fullscreen)))
           (set-frame-parameter nil 'fullscreen
                                (if (equal 'fullboth current-value)
                                    (if (boundp 'old-fullscreen) old-fullscreen nil)
                                    (progn (setq old-fullscreen current-value)
                                           'fullboth)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end-commenting code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun char-lowercasep (c)
  (and (< c 123) (> c 96)))

(defun char-capitalp (c)
  (and (< c 91) (> c 64)))
	  
(defun decapitalize (str)
  "Opposite of capitalize function. If the first char is capital case, it will be converted to lower case."
  (let ((ls (string-to-list str)))
	(if (char-capitalp (car ls))
		(concat (cons (+ (car ls) 32) (cdr ls)))
	  str)))

(defun capitalize (str)
  "Non interactive capitalize function. If the first char is capital case, it will be converted to lower case."
  (let ((ls (string-to-list str)))
	(if (char-lowercasep (car ls))
		(concat (cons (- (car ls) 32) (cdr ls)))
	  str)))

(defun but-first (str)
  "Strip the first letter in the string"
  (concat (cdr (string-to-list str))))


(defun join (ls str)
  "Python like join"
  (mapconcat 'identity ls str))

(defun get-import-from-grep-result ()
  (defun filter-list(list result &optional seen_java)
    (if (null list)
        result
      (if (not seen_java)
          (if (or (string= (car list) "com")
                  (string= (car list) "org"))
              (filter-list list result t)
            (filter-list (cdr list) result))
        (filter-list (cdr list) (cons (car list) result) seen_java))))
  (let* ((buf (buffer-substring (line-beginning-position) (line-end-position)))
         (filename (car (split-string buf ":")))
         (splits (split-string filename "/"))
         (result (concat "import " (replace-regexp-in-string "\\.java" "" (join (reverse (filter-list splits '())) ".")) ";")))
    (message (concat "import is "  result))
    result))

(defun add-to-imports()
  (interactive)
  (let ((import (get-import-from-grep-result)))
    (pop-to-buffer (first (buffer-list)))
    (save-excursion
      (goto-char (point-max))
      (search-backward-regexp "import ")
      (line-move 1)
      (insert import)
      (insert "\n"))))

(defun javadoc-method-comment () 
  ;; Insert a javadoc method comment at the cursor position 
  (interactive) 
  (insert 
"/** 
 * 
 * 
 * 
 * 
 * @param 
 * @return
 * @exeption
 * @see 
 */ 
") 
  (forward-line -8) 
  (end-of-line))


(defun prepend-to-line (s)
  (beginning-of-line)
  (insert s))

(defun line-count (start-point end-point)
  (save-excursion
    (goto-char start-point)
    (let ((start-line (line-number-at-pos)))
      (goto-char end-point)
      (- (line-number-at-pos) start-line))))
          
(defun javadoc-comment ()
  (interactive)
  (with-current-buffer
      (let ((b (region-beginning))
            (e (region-end)))
        (if (= b e)
            (message "%s" "Please select a region")
          (progn
            (goto-char b)
            (insert "/** ")
            (line-move 1)
            (loop 
             for line-number from 0 to (- (line-count b e) 1)
             do 
             (prepend-to-line "  * ")
             (line-move 1))
            (prepend-to-line "  */\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Generate getters  ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun gen-getter(vartype varname acc-spec)
  "Generates a getter"
  (interactive "sVartype: \nsVarname: \nsAccess specifier: ")
  (with-current-buffer
	  (progn
		(insert (concat acc-spec " " vartype " get" (but-first varname) "()"))
		(insert "\n{\n")
		(insert (concat "\treturn " varname ";"))
		(insert "\n}\n"))))


(defun block-site (site)
  "Blocks a site"
  (interactive "sPlease enter the site to block: ")
  (save-excursion
	(if (equal system-type 'windows-nt)
		(find-file "C:\\Windows\\system32\\drivers\\etc\\hosts")
	  (find-file "/sudo::/etc/hosts"))
	(if (eq (search-forward site nil t) nil)
		(progn
		  (goto-char (point-max))
		  (insert (concat "127.0.0.1\t\t" site))
		  (newline)
		  (insert (concat "127.0.0.1\t\twww." site))
		  (newline)
		  (save-buffer))
	  (print (concat "Site " site  " is already in hosts file")))
	(kill-buffer)))


;; (replace-regexp-in-string ";" "" (replace-regexp-in-string "\\." "/" (cadr (split-string "import a.b.c;"))))
(defun switch-to-h-file ()
  "switch to corresponding .h/.cpp file"
  (interactive)
   (let	((filename (reverse (string-to-list (buffer-file-name)))))
	 (if (= (car filename) 112)
		 (find-file (concat (reverse (cons 104 (cdddr filename)))))
	   (if (= (car filename) 104)
		   (find-file (concat (reverse (append '(112 112 99) (cdr filename)))))
		 '()))))
   

(defun foldr (fn seed args)
  (if (equal args '()) 
	  seed
	(foldr fn (funcall fn (car args) seed) (cdr args))))


(defun get-arg-list (str)
  (let ((ls (foldr (lambda (x res)
		   (if (= x 59) 
			   (cons (+ 1 (car res)) res)
			 res)) 
		 (list 0) (string-to-list str))))
	(foldr (lambda (x res)
			 (concat (number-to-string x) "," res)) (number-to-string (car ls)) (cdr ls))))

;Generates well rendered diagrams from ascii art.
(setq ditaa-cmd "java -jar C:/apps/ditaa.jar -E")
(defun ditaa-generate ()
  (interactive)
  (shell-command
    (concat ditaa-cmd " " buffer-file-name)))

(defun ack (pattern)
  (interactive "sAck grep for: ")
  (shell-command (concat "ack-grep -i "  pattern " &")))

(defun grep-it (pattern)
  (interactive "sGrep for: ")
  (grep (concat " -nH -e " pattern "*"))) 

(defun recursive-grep (pattern)
  (interactive "sRecursive Grep for: ")
  (rgrep (concat "-nH -e " pattern " *")))

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun decamelify-char (c)
  (if (char-capitalp c)
	  (concat "_" (char-to-string c))
	(if (char-lowercasep c)
		(char-to-string (- c 32))
	  (char-to-string c))))

(defun decamelify-label (str)
  (apply #'concat (mapcar #'decamelify-char (string-to-list (decapitalize str)))))

(defun decamelify-labels  ()
  "Decamelify all labels(string which end with colon)"
  (interactive)
  (let (cb label)
  (with-current-buffer cb
	(setq label (search-forward ":"))) 
	  label))

(defun jad ()
  "Run java disassembler on the current buffer"
  (shell-command "jad" (buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close all emacs buffers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Xahs documentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun xah-grep (xah-search)
  "Search for a key word in xah's documentation"
  (interactive "sxah-search:")
  (let ((results (concat temporary-file-directory "xah_results.html")))
	(start-file-process-shell-command  "Xah Grep" "Xah results" 
									   (concat "find "  xah-doc-location "| xargs grep -nHe " xah-search  " > " results))
	(pop-to-buffer "Xah results")
	(browse-url (concat "file:///" results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enough beauty, use chrome ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun browse-url-chrome(url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((out-buf "*Messages*"))
	(start-process "chrome" out-buf "C:/Documents and Settings/QA_Testing/Local Settings/Application Data/Google/Chrome/Application/chrome.exe" url)))

;;;;;;;;;;;;;;;;;;;;;;;
;; browser for linux ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun browse-url-linux(url &optional new-window)
  (interactive (browse-url-interactive-arg "URL:"))
  (let ((out-buf "*Messages*"))
	(start-process "browser" out-buf "google-chrome" url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The beautiful Conkeror browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun browse-url-conkeror-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((out-buf "*Messages*"))
	(start-process "conkeror" out-buf "C:/apps/xulrunner/conkeror.bat" url)
	(pop-to-buffer out-buf)))

;; TODO: Fix this.. breaks shit
;; (defun clean-emacs-temps ()
;;   (interactive)
;;   (let* ((bufs (list-buffers-noselect))
;;          (files (mapcar buffer-file-name bufs)))
;;     (mapcar (lambda(filename) 
;;               (let (possibles (list (concat filename "~")
;;                                     (concat filename "#")
;;                                     (concat "#" filename "#")))
;;                 (mapcar (lambda (possible)
;;                           (if (file-exists-p possible)
;;                               (progn
;;                                 ;; (format t (concat "Deleting file " possible))
;;                                 (delete-file possible))))
;;                         possibles)))
;;          files)))
                 

;; (buffer-file-name (caddr (buffer-list)))
;; (mapcar buffer-file-name (buffer-list))
;; (clean-emacs-temps)


;;;;;;;;;;;;;;
;; winmerge ;;
;;;;;;;;;;;;;;
(defun wdiff (buffer1 buffer2)
  (interactive "sbuffer1:\nsbuffer2:")
  (let ((tempdir "C:/WINDOWS/Temp/")
		(winmerge "C:/Program Files/WinMerge/WinMergeU.exe"))
	(let ((buf1 (get-buffer-create buffer1))
		  (buf2 (get-buffer-create buffer2))
		  (tempfile1 (buffer-file-name (get-buffer-create buffer1)))
		  (tempfile2 (buffer-file-name (get-buffer-create buffer2))))
	  (start-process "winmerge" nil winmerge tempfile1 tempfile2))))

(cond ((equal system-type 'windows-nt)
		(setq browse-url-browser-function 'browse-url-chrome))
	   ((equal system-type 'gnu/linux)
		(setq browse-url-browser-function 'browse-url-linux)))

(defun run-cmd (mode name buffer-name cmd &rest params)
  (progn
    (if (not (eq (get-buffer buffer-name) nil))
        (kill-buffer buffer-name))
    (let* ((out-buf (get-buffer-create buffer-name))
           (args (foldr 'cons params (list cmd
                                           out-buf
                                           name))))
      (set-buffer out-buf)
      (funcall mode)
      (switch-to-buffer-other-window out-buf 1) 
      (apply 'start-process args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show methods in the current buffer. Works only for java/C++ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun show-methods ()
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (progn
      (message "In show methods")
      (run-cmd 'grep-mode 
               "grep " 
               "*methods*" 
               "grep" 
               "-E" 
               "(public|private|protected).*\(.*\)$" 
               file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and open if you only have only file, else show the output in buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ll-code-dir nil)
(defun search-index-internal (file str)
  (if (null ll-code-dir)
      (setq ll-code-dir (read-from-minibuffer "Please set a global search dir:")))
  (run-cmd  'grep-mode 
	    "searcher" 
	    "*index-searcher*" 
	    "/Users/ravi/ll/bin/search.sh" 
	    ll-code-dir
	    file str))

(defun search-index(file str)
  (interactive "sFile Pattern:\nsString Pattern:")
  (search-index-internal file str))

(defun word-at-point ()
  ;; (interactive)
  (with-current-buffer
      (progn
        (backward-word)
        (let ((word-begin (region-beginning)))
          (forward-word)
          (let ((word-end (region-end)))
            (buffer-substring word-begin word-end))))))

(defun search-index-at-point()
  (interactive)
  (let ((word (word-at-point)))
    (progn
      (message (format "The word is %s" word))
      (search-index-internal "*.java$" word))))

(defun dos2unix (buffer)
      "Automate M-% C-q C-m RET C-q C-j RET"
      (interactive "*b")
      (save-excursion
        (goto-char (point-min))
        (while (search-forward (string ?\C-m) nil t)
          (replace-match (string ?\C-j) nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile and install ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun compile-install(&optional compiletype reinstall)
;;   (interactive ("sCompile[core|full|clean]:\nsReinstall[y|N]:"))
;;   (let ((comp-cmd "make -C/ext/home/ravi/project/finder_sprint/trunk "))
;;     (cond ((equal compiletype "core")
;;            (compile 
;;   (run-cmd "compile_and_run" "*compile_and_run*"

(defun open-fileline ()
  "Open a file from the path in the region"
  (interactive)
  (let* ((buf (buffer-substring (line-beginning-position) (line-end-position)))
         (splits (split-string buf ":"))
         (filename (car splits)))
    (progn
     (find-file filename)
     (if (> (length splits) 1)
         (let ((linenumber (string-to-number (cadr splits))))
           (progn
             (goto-char (point-min))
             (forward-line (1- linenumber))))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Java-like string replace ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
   All arguments are strings.
   When optional fourth argument is non-nil, treat the from as a regular expression."
  (progn
	(setq case-fold-search nil)
	(let ((pos 0)
		  (res "")
		  (from (if re from (regexp-quote from))))
	  (while (< pos (length string))
       (let ((beg (string-match from string pos)))
         (if (not beg) 
	     (progn
	       (setq res (concat res
				 (substring string pos (match-beginning 0))
								to))
			  (setq pos (match-end 0))
			  (setq case-fold-search t))
		  (progn
			(setq res (concat res (substring string pos (length string))))
			(setq pos (length string))
			(setq case-fold-search t))))
    res))))


;;;;;;;;;;;;;;;;;
;; Test only.  ;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given a converter method that converts Type A to Type B, generate a converter ;;
;; that converts from List<A> to List<B>                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun printls (ls)
    (let ((str (concat (apply 'concat ls) "\n")))
      (insert str)
      str))

(defun mp (a b c)
  (interactive "sa:\nsb:\nsc:")
  (mapcar 'printls (list (list a b c) (list a b c))))

(defun java-list (tA tB convert)
  (interactive "sTypeA:\nsTypeB:\nsConverter:")
  (let* ((varA (decapitalize tA))
         (lsA (concat varA "s"))
         (varB (decapitalize tB))
         (lsB (concat varB "s"))
         (nl  "\n")
         (code 
          "public List<tB> converts(List<tA> lsA) {
              if (lsA == null) {
                 return null;
              }
               
              List<tB> lsB = new ArrayList<tB>();
              for(varA: lsA) {
                 lsB.add(convert(varA));
              }  
           }"))
         (insert 
          (string-replace "tA" tA 
                         (string-replace "tB" tB
                                         (string-replace "convert" convert code))))))
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun sub-str(str from to)
  (if (> (- to from) (length str))
      str
    (substring str from to)))

(defun open-local-file (filename &optional refresh)
  (switch-to-buffer (find-file-noselect filename))
  (if refresh 
      (forward-line -1)
      (auto-revert-tail-mode)))

(defun open-org ()
  (interactive)
  (open-local-file "~/ll/others/notes/locationlabs.org"))


(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;(defun switch-window transpose-window-orientation)

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))


(provide 'misc)





    





