(setq replay-root "C:/projects/replay_pegasus")
(setq replay-copyright-str "//////////////////////////////////////////////////////////////////////////
//
// Confidential - Copyright (C) 2004-2011 Replay Solutions, Inc.
//
// These coded instructions, statements and computer programs contain
// unpublished information proprietary to Replay Solutions and are
// protected by US federal copyright laws. They may not be disclosed to
// third parties or copied or duplicated, in whole or in part, without
// prior written consent of Replay Solutions. Unpublished-rights reserved
// under the Copyright Laws of the United States.
//
//
//////////////////////////////////////////////////////////////////////////
")

(defun open-index ()
  (interactive)
  (find-file-read-only (concat replay-root "/index")))


(defun set-replay-root (version close)
  (interactive "sReplay version:\nsClose Previous Buffers[Y/n]:")
  (progn
	(cond ((equal close "n") '())
		  (t (close-all-buffers)))
	(cond ((equal system-type 'windows-nt)
		   (setq replay-root (concat "C:\\projects\\replay_" version)))
		  ((equal system-type 'gnu/linux)
		   (setq replay-root (concat "/home/ravi/projects/replay_" version))))
	(open-index)
	(print (concat "Replay root is now " replay-root))))

(defun gen-index()
  (interactive)
  (let ((script ""))
	(progn
	  (cond ((equal system-type 'windows-nt)
			 (setq script "gen-index.bat"))
			((equal system-type 'gnu/linux)
			 (setq script "./gen-index.sh")))
	  (shell-command (concat script " " replay-root  " &")))))



(defun gen-java-generic-fn (start end)
  (interactive "r")
  (let ((types '("char" "int" "short" "long" "float" "double"))
		(region (string-replace "<X> " "" (buffer-substring start end))))
	(loop for type in types do
		  (insert (concat "\n" (string-replace "X" type region) "\n")))))

(defun nsis-create-macro (name)
  (interactive "sMacroname: ")
  (with-current-buffer
	  (progn
		(insert (concat "!define " name " \"!insertmacro " name"Mac\"\n"))
		(insert (concat "!macro "name"Mac\n!macroend")))))

(defun bci-insert-method-call (name sig target)
  (interactive "sinstrument-method:\nssignature:\nstarget-method:")
  (with-current-buffer
	  (progn
		(insert "\n<Method Name=\"" name "\" Signature=\"" sig "\">")
		(insert "\n\t <BeforeCall")
		(insert "\n\t\t Class=\"com/replaysolutions/agent/logsync/JavaLoggerHandler\"")
		(insert (concat "\n\t\t Method=\"" target "\""))
		(insert (concat "\n\t\t Signature=\"(Ljava/util/logging/Logger;" (cdr (string-to-list sig)) "\""))
		(insert (concat "\n\t\t PassArgs=\"" (get-arg-list sig) "\"/>"))
		(insert "\n </Method>")
		"ret")))


(defun gen-jni (class returntype)
  (defun replace-dot (str)
	(apply #'concat (mapcar (lambda (x) (char-to-string (if (= x 46) 95 x)))  (string-to-list str))))
  (interactive "sclass:\nsreturntype:")
  (with-current-buffer
	  (insert "extern \"C\" JNIEXPORT " returntype " JNICALL " "Java_" (replace-dot class) "(\nJNIEnv* pEnv, jclass ....) {\n}")))  


(defun jstring->cstring (jstring cstring)
  (interactive "sjstring:\nscstring:")
	(with-current-buffer
	   (progn
		 (insert (concat "REPLAY_COPY_JSTRING(pEnv, " cstring ", " jstring ");")))
	  "ret"))

(defun cstring->jstring (cstring)
  (interactive "scstring:")
  (with-current-buffer
		(insert (concat "pEnv->NewString(" cstring ".c_str());"))))

(defun assert (condition)
  (interactive "scondition:")
  (with-current-buffer
	  (insert (concat "RELEASE_ASSERT( " condition " );" ))))

(defun singleton (interface)
  (interactive "sinterface:")
  (with-current-buffer
	  (insert (concat "GetSingleton<replay::" interface">()->"))))

(defun prefix? (a b)
  "Is string 'a' a prefix of b"
  (defun helper (as bs)
	(cond ((equal as nil) t) 
		  ((equal bs nil) nil)
		  ((= (car as) (car bs)) (helper (cdr as) (cdr bs)))
		  (t nil)))
  (helper (string-to-list a) (string-to-list b)))

(defun fulldirname (element ls)
  (cond ((eq ls nil) nil)
		((prefix? element (car ls)) (car ls))
		(t (fulldirname element (cdr ls)))))

(defun create-replay-scratch (&optional caseid)
  (interactive "scaseid:")
  (let ((replay-scratch-dir "C:/Documents and Settings/QA_Testing/My Documents/ReplayTemp/"))
	(find-file (concat replay-scratch-dir "case-" caseid))))

(defun open-replay-dir (&optional dirname)
  (interactive "sdirname:")
  (let ((replay-ent-home (concat replay-root "/ReplayEnterprise/")))
	(cond ((equal "" dirname)
		   (find-file replay-root))
		  ((equal "bin" dirname)
		   (find-file (concat replay-root "/Sandbox/Bin")))
		  ((equal "recs" dirname)
		   (find-file (concat replay-root "/Sandbox/Bin/ReplayServer/Recordings")))
		  ((equal "ReplayCore" dirname)
		   (find-file (concat replay-root "/" dirname "/Src")))
		  ((equal "ReplayServer" dirname)
		   (find-file (concat replay-root "/" dirname)))
		  ((equal "installer" dirname)
		   (find-file (concat replay-ent-home "/ReplayEnterpriseInstaller")))
		  ((equal "BuildScript" dirname)
		   (find-file (concat replay-ent-home "/BuildScript")))
		  ((equal "plugin" dirname)
		   (find-file (concat replay-ent-home "/ReplayEclipsePlugin/src/com/replaysolutions//plugin/eclipse/")))
		  (t (let ((fullname (fulldirname dirname (directory-files replay-ent-home))))
			   (if fullname (find-file (concat replay-ent-home "/" fullname "/src"))))))))

(defun open-java-file (classname project)
  "Opens a java file in replay dir. This will only work 
if the current file contains  the import to classname"
  (interactive "sclassname:\nsproject:")
  (save-excursion
	(search-backward-regexp (concat "import.*" classname ";$"))
	(open-java-file-from-import project)))
	

(defun show-import (classname)
  (interactive "sclassname: ")
  (save-excursion
	(search-backward-regexp (concat "import.*" classname ";$"))
	(print (buffer-substring (point-at-bol) (point-at-eol)))))

(defun td-my ()
  (interactive)
  (with-current-buffer
	  (insert "//TODO (RKG)")))

(defun sign-c ()
  (interactive)
  (with-current-buffer
		(insert "(RKG) ")))

(defun open-java-file-from-import (project)
  (let ((prefix (concat replay-root "/ReplayEnterprise")))
		(import-name (buffer-substring (point-at-bol) (point-at-eol))))
	(find-file (concat prefix project "/src/" (replace-regexp-in-string ";" "" (replace-regexp-in-string "\\." "/" (cadr (split-string import-name)))) ".java" )))


(defun replay-copyright ()
  (interactive)
  (save-excursion
	(progn
	  (goto-char (point-min))
	  (insert replay-copyright-str))))
	  

(setq c-mode-hook
	  '(lambda ()
		 (gtags-mode 1)
		 ))

