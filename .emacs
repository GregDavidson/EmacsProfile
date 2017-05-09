; Except for loading init.el below, this should
; all be code added automatically by Emacs'
; customization systems.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-paren-distance 300000)
 '(browse-url-browser-function (quote eww-browse-url))
 '(buffer-face-mode-face (quote fixed-pitch))
 '(custom-buffer-indent 8)
 '(delete-selection-mode nil)
 '(desktop-save-mode t)
 '(dired-listing-switches "-alB")
 '(fill-column 60)
 '(lisp-body-indent 2)
 '(lisp-indent-maximum-backtracking 4)
 '(lisp-indent-offset 2)
 '(lisp-loop-forms-indentation 2)
 '(lisp-loop-keyword-indentation 2)
 '(lisp-simple-loop-indentation 2)
 '(lisp-tag-body-indentation 2)
 '(mark-even-if-inactive t)
 '(org-agenda-files (quote ("~/Notes/Languages/javascript.org")))
 '(org-log-done (quote time))
	'(package-selected-packages
		 (quote
			 (git-commit perl6-mode perspective org-autolist org-bullets org-page org-projectile org-ref org-tree-slide tldr toc-org workgroups2 typed-clojure-mode smex scala-mode2 scala-mode rainbow-delimiters prolog project-mode php-mode paredit-everywhere multi-term ido-ubiquitous idle-highlight-mode helm ghci-completion flycheck-rust flycheck-haskell flycheck-clojure find-file-in-project enh-ruby-mode ecb-snapshot company-racer closure-lint-mode clojure-mode-extra-font-locking cljsbuild-mode cljdoc auto-complete)))
 '(pr-faces-p t)
	'(pr-ps-printer-alist
		 (quote
			 ((hpdeskjet940c "lpr" nil "-P" "lpr")
				 (bilbo "lpr" nil "-P" "lpr"))))
 '(pr-txt-printer-alist (quote ((hpdeskjet940c "lpr" nil "lpr"))))
	'(prolog-program-name
		 (quote
			 (((getenv "EPROLOG")
					(eval
						(getenv "EPROLOG")))
				 (eclipse "eclipse")
				 (mercury nil)
				 (sicstus "sicstus")
				 (swi "prolog")
				 (gnu "gprolog")
				 (t "prolog"))))
 '(ps-font-size 9.0)
 '(ps-n-up-border-p nil)
 '(ps-n-up-printing 1)
 '(ps-number-of-columns 1)
 '(ps-print-header t)
 '(rust-indent-offset 2)
	'(safe-local-variable-values
		 (quote
			 ((c-hanging-comment-ender-p)
				 (outline-minor-mode . 1)
				 (folded-file . t)
				 (folding-internal-margins))))
 '(scheme-program-name "guile")
 '(scroll-bar-mode (quote right))
 '(select-active-regions t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(sql-database "wicci1")
 '(sql-product (quote postgres))
 '(sql-user "greg")
 '(standard-indent 2)
 '(tab-width 2)
 '(tcl-application "tclsh")
 '(tcl-continued-indent-level 8)
 '(tcl-indent-level 8)
 '(tool-bar-mode nil)
 '(tramp-default-host "nil")
 '(tramp-default-host-alist nil nil (tramp))
 '(transient-mark-mode 1)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(xsl-element-indent-step 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "brown"))))
 '(j-adverb-face ((t (:foreground "Green"))))
 '(j-conjunction-face ((t (:foreground "Blue"))))
 '(j-other-face ((t (:foreground "Black"))))
 '(j-verb-face ((t (:foreground "Red")))))

; Enable some default-disabled commands

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(load "~/.emacs.d/init.el")
