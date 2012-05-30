					;See this link for what these syntax variables mean
					;http://www.phys.ufl.edu/docs/emacs/emacs_252.html#SEC258
					;set c-style (proper indentation)
(c-add-style "my-c-style"
	     '((c-basic-offset  . 3)
	       (c-comment-only-line-offset . 0)
	       (c-hanging-braces-alist . ((brace-list-open)
					  (brace-entry-open)
					  (substatement-open after)
					  (block-close . c-snug-do-while)
					  (arglist-cont-nonempty)))
	       (c-cleanup-list . (brace-else-brace))
	       (c-offsets-alist . ((statement-block-intro . +)
				   (knr-argdecl-intro     . 0)
				   (comment-intro         . 0)
				   (substatement-open     . 0)
				   (substatement-label    . 0)
				   (label                 . 0)
				   (statement-cont        . +)
				   (statement-case-open   . +)))))

(c-add-style  "my-java-style"
	      '((c-basic-offset . 3)
		(c-comment-only-line-offset . (0 . 0))
		;; the following preserves Javadoc starter lines
		(c-hanging-braces-alist . ((brace-list-open)
					   (brace-entry-open after)
					   (substatement-open after)
					   (block-close . c-snug-do-while)
					   (arglist-cont-nonempty)))
		(c-offsets-alist . ((inline-open . 0)
				    (topmost-intro-cont    . +)
				    (statement-block-intro . +)
				    (knr-argdecl-intro     . 5)
				    (comment-intro         . 0)
				    (substatement-open     . 0)
				    (substatement-label    . +)
				    (label                 . +)
				    (statement-case-open   . 0)
				    (statement-cont        . 0)
				    (arglist-intro  . c-lineup-arglist-intro-after-paren)
				    (arglist-close  . c-lineup-arglist)
				    (access-label   . 0)
				    (inher-cont     . c-lineup-java-inher)
				    (func-decl-cont . c-lineup-java-throws)))))

(setq c-default-style
      '((java-mode . "my-java-style") (other . "my-c-style")))


(setq c-mode-hook
      '(lambda ()
	 (gtags-mode 1)
	 ))


;; (add-hook 'java-mode-hook (function cscope:hook))
(add-hook 'java-mode-hook
          (lambda ()
            "Treat Java 1.5 @-style annotations as comments."
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)))

(provide 'language-styles)
