;; This file is automatically edited by the
;; Emacs Customization System.
;; Be very careful if you hand-edit this file!!

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
 '(paperless-capture-directory "~/Scan/ScansNew")
 '(paperless-root-directory "/More/Greg/Archives/2017")
 '(pr-faces-p t)
	'(pr-ps-printer-alist
		 (quote
			 ((hpdeskjet940c "lpr" nil "-P" "lpr")
				 (bilbo "lpr" nil "-P" "lpr"))))
 '(pr-txt-printer-alist (quote ((hpdeskjet940c "lpr" nil "lpr"))))
 '(printer-name nil)
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
 '(ps-printer-name "hp-color-laser")
 '(ps-printer-name-option "-P")
 '(rust-indent-offset 2)
	'(safe-local-variable-values
		 (quote
			 ((sql-directory-path . "/ssh:ngender.org:")
				 (directory-path . "/ssh:ngender.org:")
				 (syntax . common-lisp)
				 (package . maxima)
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
 '(tramp-default-host "nil" nil (tramp))
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