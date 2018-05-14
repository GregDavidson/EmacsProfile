;; * Org Mode Support
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(defvar *ngender-org-packages* '(mysql-to-org org-autolist org-bullets org-page
	org-projectile org-tree-slide ox-gfm toc-org)
  "packages to support org-mode" )

;; Ensure org package is on the list at the front:
(setq *ngender-org-packages*
  (delete-dups (cons 'org (ngender-symbol-value '*ngender-org-packages*))) )

;; Ensure all of those packages have been installed
(apply #'ngender-package-function *ngender-org-packages* )

;; Require the features in order to load the packages
(require 'org)

;; ** Now configure org nicely

(setq org-directory (expand-file-name "~/Notes"))
(setq org-default-notes-file (expand-file-name "Notes" org-directory))

(defconst my-org-babel-t
;;	'(emacs-lisp shell awk sed sql J scheme clojure prolog)
;;	'(emacs-lisp shell awk sed sql J scheme clojure)
	'(emacs-lisp awk sql J scheme clojure)
	"known org-mode source block languages I'd like org to evaluate" )

;; More org-babel packages, would allow more babel-t languages:
;; tcl http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-tcl.html
;; org-babel-eval-in-repl melpa Eval org-mode babel code blocks in various REPLs.
;; ob-sagemath melpa org-babel functions for SageMath evaluation
;; ob-rust melpa Org-babel functions for Rust
;; ob-typescript https://github.com/lurdan/ob-typescript
;; ob-max for maxima

(defconst my-org-babel-nil
	'(C C++ calc css dot gnuplot haskell js latex lisp max oz perl python rust tcl)
	"source block languages I want org to prompt for, but not to try to evalute" )

(defconst my-org-babel-names
	(mapcar #'symbol-name (append my-org-babel-t my-org-babel-nil))
	"all these languages as strings for org-insert-source-block to prompt for" )

;; the emacs-lisp compiler is trying to evaluate y at compile time in:
;; (defconst my-org-babel
;; 	(cl-flet ( (pair-with (y) (lambda (x) (cons x y))) )
;; 		(append (mapcar (pair-with t) my-org-babel-t) (mapcar (pair-with nil) my-org-babel-nil) ) )
;; 	"See http://orgmode.org/manual/Languages.html#Languages and what I do with this value" )

;; so here it is expanded:
(defconst my-org-babel
	(append (mapcar (lambda (x) (cons x t)) my-org-babel-t) (mapcar (lambda (x) (cons x nil)) my-org-babel-nil) )
	"See http://orgmode.org/manual/Languages.html#Languages and what I do with this value" )

;; don't we want set comparison??  Order matters!!
(unless (equal (mapcar #'car my-org-babel) (mapcar #'car org-babel-load-languages))
	(org-babel-do-load-languages 'org-babel-load-languages my-org-babel) )

;; http://wenshanren.org/?p=334 Friday 18 November 2016 s/src/source/
(defun org-insert-source-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
			(list (ido-completing-read "Source code type: " my-org-babel-names)))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (forward-line -2)
    (org-edit-src-code) ) )

(global-set-key "\C-c'" 'org-insert-source-block)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org-mode-ngender ()
	(set-face-attribute 'org-table nil :inherit 'fixed-pitch) )

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-mode-ngender)
(add-hook 'org-mode-hook 'ngender-pitch-mode)

;; ** Provide

(ngender-provide org)
