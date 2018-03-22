;;; ~/.emacs.d/jgd/jgd.el - my emacs worth compiling
;; J. Greg Davidson

;; Settings and keybindings are at the bottom of this file;
;; you'll want to look there unless you're interested in
;; emacs lisp programming.

;;; Table of Contents
;	Table of Contents
;;	Dependencies - provide and require
;;	Define some functions for managing lists, sets, paths
;;	Customize some directory path lists
;;	Define some more functions
;;	Define some interactive functions
;;	Mode-Specific Customizations
;;		Tcl Preferences
;;		C/C++ Preferences
;;		etc.
;;	Key Bindings - (Re)Define some keyboard shortcuts
;;	Set some miscellaneous variables

;;; Dependencies - provide and require

(provide 'jgd)

(setq lexical-binding t)

;; (require 'x-fonts)
(require 'jit-lock)

;;; Overall Preferences

;; Indenting by tabs
;; Problem: We want to be able to change the tab-width
;; but we want all indenting to be done in tabs.  There
;; seems to be an interaction as well as clumsiness here!

(defconst jgd-min-indent 1)
(defconst jgd-default-indent 2)
(defconst jgd-default-tab-width 2)

;;; Define some functions for managing lists, sets, paths

;; functions for filtering lists

(defun jgd-filter-reverse (items predicate)
  "reversed sublist of items which satisfy predicate"
  (let (results)
    (dolist (item items results)
      (or (apply predicate item) (push item results)) ) ) )

(defun jgd-update-filter-reverse (symbol predicate)
  (set symbol (jgd-filter-reverse (symbol-value symbol) predicate)) )

(defun jgd-filter (predicate items)
  "sublist of items which satisfy predicate"
  (let* ( (results (list 'results)) (tail results) )
    (dolist (item items (cdr results))
      (when (apply predicate item)
	(setcdr tail (list item))
	(setq tail (cdr tail)) ) ) ) )

(defun jgd-update-filter (symbol predicate)
  (set symbol (jgd-filter (symbol-value symbol) predicate)) )

;; a list of elements can be used as a set

(defun jgd-union-list (bag items)
  "union of bag and items"
  (dolist (item items bag)
      (add-to-list 'bag item ) ) )

(defun jgd-update-union (symbol &rest items)
  (set symbol (jgd-union-list (symbol-value symbol) items)) )


;; functions for managing paths

;; Would be better to have a filter
;; which would remove and warn
;; of any element already on a path
;; or non-existent in the filesystem.
;; Then just append in front or in back.

(defun jgd-add-paths (symbol paths &optional append)
  "add paths onto existing unique list bound to symbol"
  (dolist (p paths)
      (or (not (file-directory-p p))
				(add-to-list symbol (expand-file-name p) append) ) ) )

(defun jgd-prepend-paths (symbol &rest dirs)
	(or (boundp symbol) (set symbol '()))
	;; assert symbol bound to proper list!!
	(jgd-add-paths symbol dirs) )

(defun jgd-append-paths (symbol &rest dirs)
	(or (boundp symbol) (set symbol '()))
	;; assert symbol bound to proper list!!
	(jgd-add-paths symbol dirs t) )

(defun jgd-str-member (str item &optional delim)
  "is item a member of delimited string?"
  (let ( (d (or delim ":")) )
    (string-match (concat ".*" delim (regexp-quote item) delim ".*")
		  (concat delim str delim) ) ) )

(defun jgd-str-append (str item &optional delim)
  "append item onto delimited string"
  (concat str (or delim ":") item) )

(defun jgd-update-str-append (symbol item &optional delim)
  (set symbol (jgd-str-append (symbol-value 'symbol) item delim)) )

(defun jgd-exec-path-from-shell ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)) ) )

;;; Customize some directory path lists

;; (jgd-prepend-paths (if (boundp 'Info-directory-list)
;; 		     'Info-directory-list 'Info-default-directory-list)
(jgd-prepend-paths 'Info-directory-list
              "/usr/share/info/"
              "/usr/share/info/emacs-24/"
              "/usr/local/share/info/"
              "/usr/share/texmf/doc/info/"
;;              "/usr/local/mercury-0.11.1-beta-2004-06-30/info"
;;              "/opt/gnome/share/info"
              )

(jgd-append-paths 'load-path
	"/usr/share/emacs/site-lisp/"
	"/usr/share/emacs/site-lisp/w3m/"
	)

(jgd-prepend-paths 'load-path
              "~/Lib/Emacs/"
              "~/Lib/Emacs/Scala/"
              "/usr/share/emacs/site-lisp"
              "/usr/share/emacs/site-lisp/w3"
              "/usr/share/emacs/site-lisp/w3m"
;;              "/data/greg/Lib/Emacs/lisp"
;;              "/usr/local/Mercury/lib/mercury/elisp"
;;              "/usr/local/src/flora2/emacs"
;;              "/usr/local/src/XSB/etc"
;;              "/usr/share/doc/ruby-1.6.4/misc"
              )

;;; Define some more functions

(defun toggle-fullscreen ()
	"Why does this toggle?"
	(interactive)
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
		'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0) )
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
		'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0) )
)
(global-set-key [?\s-f] 'toggle-fullscreen)

(defun jgd-split-window (n)
	(interactive "p")
	(if (and (= n 1) (boundp 'jgd-num-cols))
		(setq n jgd-num-cols) )
	(toggle-fullscreen)
	(cond
		(	(> n 1)
			(split-window-horizontally)
			(balance-windows)
			(jgd-split-window (- n 1))
		)
		(	(< n -1)
			(split-window-vertically)
			(balance-windows)
			(jgd-split-window (- n 1))
		)
)	)
(global-set-key [?\s-m] 'jgd-split-window)

;; Are we running under a window-system?
;; The variable window-system will be nil if not,
;; otherwise it will tell us which gui we're using!
(defun using-gui-p () window-system)

;; ideally minor would be optional, and maybe a sub-minor
;; could be allowed as well?
(defun version>= (major minor)
  (or (> emacs-major-version major)
	  (and (= emacs-major-version major)
		   (>= emacs-minor-version minor) ) ) )

;;; tab issues

(defun jgd-set-tab-width (n)
  "Set tab-width as a local variable."
  (make-local-variable 'tab-width)
  (setq tab-width (if (< n jgd-min-indent) jgd-default-tab-width n) )
)

(defun jgd-set-default-tab-width ()
  "Set tab-width to my favorite default"
  (jgd-set-tab-width jgd-default-tab-width)
)

(defun jgd-tab-width (n)
  "Set tab-width interactively."
  (interactive "p")
  (jgd-set-tab-width n)
)

;;; Font Issues

;; Variable Pitch Mode for Source Code

(defun variable-pitch-mode-jgd ()
	(setq variable-pitch-mode 1)
)

;; Associate a decent font with the current frame.
;; The prefix argument specifies the font size, default 14.
(defun jgd-frame-font (n)
  "Set a readable non-proportional frame font."
  (interactive "p")
  (set-frame-font
   (format
		 "Dejavu Sans Mono-%d"
		 (if (< n 4) 8 n) )
		't )
)
;		 "Dejavu Sans Mono-%d"
;		 "-adobe-courier-medium-r-normal--%d-*-*-*-m-*-iso8859-1"

;;; Define some interactive functions

;; demo getting a numeric argument
(defun show-interactive-number (n)
  "show interactive number"
  (interactive "p")
  (if (boundp 'n) (message "%d" n) (message "void") )
)

;; The standard command "compile" defaults to running "make -k".
;; Define "make" to do the same thing but without the "-k".
;; Darn - it is not working the same way.  Fix it, and when it
;; works, use it instead of compile in the keybinding below.
;; Alas, this doesn't work!!!
(defun make (n)
  (interactive)
  (compile "make") )

;; Insert the date (and optionally the time) in a nice format.
;; - Invoke with Super-t, i.e. "Windows Button" + t
;; - Hold down Meta (aka Alt) and type a digit to select the
;;   desired format before running this function.
;; - The day of the week will be blank padded, which will
;;   be wierd when the day of the week is 1-9.  This should
;;   be fixed!!!
;; - It would be cool to rationalize these features into a
;;   bitset interpretation of the arguments!
(defun jgd-time (n)
  (interactive "p")
  (insert (format-time-string (cond ( (= n 0) "%e %B %Y" )
                                    ( (= n 1) "%A %e %B %Y" )
                                    ( (= n 2) "%A, %e %B %Y" )
                                    ( (= n 3) "%l:%M%p %A %e %B %Y" )
                                    ( (= n 4) "%Y-%m-%d %T%z %A" )
                                    ))) )

;; What shall I bind this to?
(defun jgd-next-window (n)
  (interactive "p")
  (other-window n)
)

;; What shall I bind this to?
(defun jgd-previous-window (n)
  (interactive "p")
  (other-window (- n))
)

;;; Themes

;; Themes
(jgd-prepend-paths 'custom-theme-load-path "~/.emacs.d/themes")
(jgd-prepend-paths 'load-path "~/.emacs.d/themes")
;(load-theme 'tomorrow-night-bright t); example - not one I like!

;;; Tcl Preferences

(require 'tcl)

(defun jgd-tcl-preferences ()
  (setq tcl-application "tclsh"
	tcl-indent-level jgd-default-indent
	tcl-continued-indent-level jgd-default-indent
	tcl-use-smart-word-finder t)  )

(add-hook 'tcl-mode-hook 'variable-pitch-mode-jgd)

;;; Shell Mode Preferences

(defun jgd-sh-mode-hook ()
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
	(setq orgstruct-heading-prefix-regexp "##* ")
	(orgstruct-mode)
)

(add-hook 'sh-mode-hook 'jgd-sh-mode-hook)
(add-hook 'sh-mode-hook 'variable-pitch-mode-jgd)

;;; C/C++ Preferences

(require 'cc-mode)

(defconst jgd-c-style
  `(
    (c-tab-always-indent . t)
    (c-comment-only-line-offset . ,jgd-default-indent) ; was 4
    (c-hanging-braces-alist . (
			       (substatement-open after)
			       (brace-list-open) ))
    (c-hanging-colons-alist . (
			       (member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after) ))
    (c-cleanup-list . (
		       scope-operator
		       empty-defun-braces
		       defun-close-semi ))
    (c-offsets-alist . (
			(arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(case-label . ,jgd-default-indent) ; was 4
			(block-open . 0)
			(knr-argdecl-intro . -) ))
    (c-echo-syntactic-information-p . t)
    (truncate-partial-width-windows . nil)
    ;; semantic bovinator CEDET features:
    ;; (add-to-list 'ac-sources '(ac-source-semantic) nil t)
    ;; (local-set-key (kbd "RET") 'newline-and-indent)
    ;; (linum-mode t)
    ;; (semantic-mode t)
    )
  "C Programming Style for Virtual Infinity Systems" )

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun jgd-c-mode-common-hook ()
  (setq c-basic-offset jgd-default-indent
	tab-width jgd-default-indent
	indent-tabs-mode t
	c-echo-syntactic-information-p t ;show parse on indent
  )
  ;; add style JGD and set it for the current buffer
  (c-add-style "JGD" jgd-c-style t)
  ;; offset customizations not in jgd-c-style
  ;; (c-set-offset 'member-init-intro '++)
  ;; other customizations
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; keybindings for supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map,
	;; objc-mode-map, java-mode-map, and idl-mode-map
	;; all inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
	(setq orgstruct-heading-prefix-regexp "/[/*] ")
	(orgstruct-mode)
)

;; PHP Mode

(defun pear/php-mode-init()
  "Set some buffer-local variables."
  (setq
		case-fold-search t
		indent-tabs-mode t
		c-basic-offset jgd-default-indent
		tab-width jgd-default-indent
		c-echo-syntactic-information-p t ;show parse on indent
		orgstruct-heading-prefix-regexp "/[/*] "
  )
	(orgstruct-mode)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
)
(add-hook 'php-mode-hook 'pear/php-mode-init)
(add-hook 'php-mode-hook 'variable-pitch-mode-jgd)

;;; Prolog preferences

;; see http://turing.ubishops.ca/home/bruda/emacs-prolog/
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog"
;; 	"Major mode for editing Prolog programs." t )
;; (autoload 'mercury-mode "prolog"
;; 	"Major mode for editing Mercury programs." t )
;; (defvar prolog-system 'swi)
;; (jgd-update-union 'auto-mode-alist
;; 		  '("\\.pl\\'" . prolog-mode)
;; 		  '("\\.m\\'" . mercury-mode) )

;;; Mozart/Oz preferences

;; (when (not (boundp 'ozhome))
;;   (defvar ozhome (or (getenv "OZHOME") (expand-file-name "/usr/lib/mozart")) )
;;   (defvar oz-bin (expand-file-name "bin" ozhome))
;;   (defvar oz-elisp (expand-file-name "share/elisp" ozhome))
;;   (when (file-directory-p ozhome)
;;     (when (and (file-directory-p oz-bin)
;; 	       (not (member exec-path oz-bin)) )
;;       (nconc exec-path (list oz-bin))
;;       (setenv "PATH" (concat (getenv "PATH") ":" oz-bin)) )
;;   (jgd-prepend-paths 'load-path oz-elisp)
  
;;   (jgd-update-union 'auto-mode-alist
;; 		    '("\\.oz\\'" . oz-mode)
;; 		    '("\\.ozg\\'" . oz-gump-mode) )
  
;;   (autoload 'run-oz "oz" "" t)
;;   (autoload 'oz-mode "oz" "" t)
;;   (autoload 'oz-gump-mode "oz" "" t)
;;   (autoload 'oz-new-buffer "oz" "" t) ) )

;;; Haskell preferences - for 2.7.0, November 2009

;; (jgd-prepend-paths 'load-path "~/Lib/Emacs/HaskellMode")

;; (load "haskell-site-file")

;; (jgd-update-union 'auto-mode-alist
;; 		  '("\\.[hg]s\\'"  . haskell-mode)
;; 		  '("\\.hi\\'"     . haskell-mode)
;; 		  '("\\.l[hg]s\\'" . literate-haskell-mode) )

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'haskell-mode-hook 'font-lock-mode)

;; Declaration scanning: just use M-x imenu or:
(global-set-key [(control meta down-mouse-3)] 'imenu)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; Older Haskell preferences (remove soon):

;; (autoload 'haskell-mode "haskell-mode"
;;   "Major mode for editing Haskell scripts." t)
;; (autoload 'literate-haskell-mode "haskell-mode"
;;   "Major mode for editing literate Haskell scripts." t)

;; adding any of the following lines according to
;; which modules you want to use:

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)

;;; CLisp Common Lisp Preferences

;; (defun slime-setup-jgd (slime-dir lisp-program)
;;   "Set up Slime to use the right Common Lisp"
;;   (jgd-prepend-paths 'load-path slime-dir)
;;   (when (and (member slime-dir load-path)
;; 	     (file-exists-p lisp-program) )
;;     (setq inferior-lisp-program lisp-program)
;;     (require 'slime)
;;     (slime-setup) ) )

;; (defun slime-setup-clisp ()
;;   (interactive)
;;   (slime-setup-jgd "~/Lib/Emacs/slime/" "/usr/bin/clisp") )

;;; Clojure nrepl Preferences

;; Based in part on the .emacs.d imported by
;; git clone https://github.com/flyingmachine/emacs-for-clojure.git ~/.emacs.d

;; (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; (setq nrepl-history-file "~/.emacs.d/nrepl-history")
;; (setq nrepl-popup-stacktraces t)
;; (setq nrepl-popup-stacktraces-in-repl t)

;; (defun pnh-clojure-mode-eldoc-hook ()
;;   (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
;;   (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;;   (nrepl-enable-on-existing-clojure-buffers)
;;   ;; prevent hippie expand from trying file names
;;   (dolist (fn 'try-complete-file-name 'try-complete-file-name-partially)
;;     (setq hippie-expand-try-functions-list (delete fn hippie-expand-try-functions-list)) )
;;   (setq ido-use-filename-at-point nil)
;;   )

;; (add-hook 'nrepl-connected-hook 'pnh-clojure-mode-eldoc-hook)

(jgd-update-union 'auto-mode-alist '("\\.cljs\\'" . clojure-mode) )

;; (add-hook 'nrepl-mode-hook 'subword-mode)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode) )

;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;;; Org Mode

(setq org-directory (expand-file-name "~/Notes"))
(setq org-default-notes-file (expand-file-name "Notes" org-directory))

(defconst my-org-babel-t
;	'(emacs-lisp shell awk sed sql J scheme clojure prolog)
	'(emacs-lisp shell awk sed sql J scheme clojure)
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

(global-set-key [?\C-\S-C ?'] 'org-insert-source-block)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org-mode-jgd ()
	(set-face-attribute 'org-table nil :inherit 'fixed-pitch) )

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-mode-jgd)
(add-hook 'org-mode-hook 'variable-pitch-mode-jgd)

;;;; Capture-Mode

;; We're capturing everything under our GTD directory
;; Maybe someday we'll separate these things out?

(global-set-key "\C-cc" 'org-capture)

(defconst gtd-dir (expand-file-name "GTD" org-directory))
(defconst gtd-inbox-org (expand-file-name "inbox.org" gtd-dir))
(defconst gtd-gtd-org (expand-file-name "gtd.org" gtd-dir))
(defconst gtd-tickler-org (expand-file-name "tickler.org" gtd-dir))
(defconst gtd-someday-org (expand-file-name "someday.org" gtd-dir))

(defconst gtd-bookmarks-org (expand-file-name "bookmarks.org" gtd-dir))
(defconst gtd-journal-org (expand-file-name "journal.org" gtd-dir))
(defconst gtd-ideas-org (expand-file-name "ideas.org" gtd-dir))
(defconst gtd-misc-org (expand-file-name "misc.org" gtd-dir))

(setq org-agenda-files (list gtd-inbox-org gtd-gtd-org gtd-tickler-org))

(setq org-capture-templates
	`( ( "t" "Todo [inbox]" entry
			 (file+headline ,gtd-inbox-org "Tasks")
			 "* TODO %i%?" )
		 ( "T" "Tickler" entry
			 (file+headline ,gtd-tickler-org "Tickler")
			 "* %i%? \n %U" )
		 ("b" "Bookmarks" entry (file+headline ,gtd-bookmarks-org "Bookmarks")
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("j" "Journal" entry (file+datetree ,gtd-journal-org)
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("i" "Ideas" entry (file+headline ,gtd-ideas-org "Ideas")
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("m" "Misc" entry (file+headline ,gtd-misc-org "Miscellany")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x")
		 ) )

;; There's some confusion about org-refile-targets
;; Initially none of these worked.  Then the first
;; one started working.  I may have needed to put
;; in a couple of levels of headings.
;; I would definitely like more flexibility and/or
;; to be able to file things to a deeper level!

(setq org-refile-targets `((,gtd-gtd-org :maxlevel . 3)
                           (,gtd-someday-org :level . 1)
                           (,gtd-tickler-org :maxlevel . 2)
                           (,gtd-bookmarks-org :level . 1)
                           (,gtd-journal-org :maxlevel . 4)
                           (,gtd-ideas-org :maxlevel . 4)
                           (,gtd-misc-org :maxlevel . 2)
))

;; (setq org-refile-targets `(
;; 			    (nil :maxlevel . 5)
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
                           
;; (setq org-refile-targets `(
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
                           

;; Rust Mode

;; More Rust Mode in init.el

(defun rust-mode-jgd ()
	(variable-pitch-mode-jgd)
	(setq orgstruct-heading-prefix-regexp "/[/*] ")
	(orgstruct-mode)
)

(add-hook 'rust-mode-hook 'rust-mode-jgd)

;; Shell Mode

(add-hook 'shell-mode-hook 'variable-pitch-mode-jgd)

;; SQL Mode

(defun sql-mode-jgd ()
	(variable-pitch-mode-jgd)
	(setq orgstruct-heading-prefix-regexp "-- ") ; maybe /* as well?
	(orgstruct-mode)
)
(add-hook 'sql-mode-hook 'sql-mode-jgd)

;;; Key Settings

;; ignore keysyms generated by my flakey Dell Studio
(global-set-key [XF86AudioMedia] 'ignore)
(global-set-key [C-XF86AudioMedia] 'ignore)
(global-set-key [M-XF86AudioMedia] 'ignore)

(global-set-key [M-left]  'beginning-of-line)
(global-set-key [M-right] 'end-of-line)
(global-set-key [M-up]    'move-to-top-of-window)
(global-set-key [M-down]  'move-to-bottom-of-window)

;; Make Control-W kill the word to the left of the cursor
;; just like it works within a shell command line.
(global-set-key [?\C-w] 'backward-kill-word)
(global-set-key [C-backspace] 'kill-region) ; was C-W

;; the "*-region-or-word" functions don't seem to exist in gnu emacs
;; (global-set-key [?\M-c] 'capitalize-region-or-word)
;; (global-set-key [?\M-C] 'upcase-region-or-word)

(global-set-key [?\s-t] 'jgd-time)
(global-set-key [?\s-c] 'upcase-word)
(global-set-key [s-prior] 'jgd-previous-window)
(global-set-key [s-next] 'jgd-next-window)
(global-set-key [?\s-\;] 'comment-region)
(global-set-key [?\s-\:] 'uncomment-region)

;; Bindings for Meta (aka Alt) plus a function key:

(global-set-key [C-f1] 'help-command)	; press f1 for help
(global-set-key [C-f2] 'shell)	; get a Unix shell in a window
(global-set-key [C-f3] 'compile)	; compile (make) something
(global-set-key [C-f4] 'next-error)	; visit the next compilation error
(global-set-key [C-f5] 'undo)		; Keep pressing it to undo more
(global-set-key [C-f7] 'blink-matching-open)	;  matching parens
(global-set-key [C-f8] 'start-kbd-macro)	; start recording commands
(global-set-key [C-f9] 'end-kbd-macro)	; finish and call it a macro
(global-set-key [C-f10] 'call-last-kbd-macro)	; re-execute the macro
;;; (global-set-key [M-f11] 'upcase-word)
(global-set-key [C-f12] 'repeat-complex-command)

;;; Set some miscellaneous variables
(setq visible-bell t)
(setq tab-stop-list nil)
(setq inhibit-startup-screen t)

;;; Require some things in ~/Lib/Emacs

;; Things from lynn.el

(defun lbd-ps-landscape ()
  "Change to landscape mode in ps"
  (interactive)
  (setq ps-landscape-mode t )
  )

(defun lbd-ps-portrait ()
  "Change to portrait mode in ps"
  (interactive)
  (setq ps-landscape-mode nil )
  )

(global-set-key [?\s-l] 'lbd-ps-landscape ) 
(global-set-key [?\s-p] 'lbd-ps-portrait ) 

(defun jgd-outline-minor-map ()
	"Create short outline-minor-mode bindings using
		Ctrl-Super- instead of Ctrl-C @."
	(interactive)
	;; Hides:
	(local-set-key [?\C-\s-q] 'hide-sublevels) ; all but top-level
	(local-set-key [?\C-\s-t] 'hide-body)	; all but headings lines
	(local-set-key [?\C-\s-o] 'hide-other) ; other branches
	(local-set-key [?\C-\s-c] 'hide-entry) ; this entry's body
	(local-set-key [?\C-\s-l] 'hide-leaves)	; body lines under this node
	(local-set-key [?\C-\s-d] 'hide-subtree) ; everything under this node
	;; Shows:
	(local-set-key [?\C-\s-a] 'show-all)	; expand everything
	(local-set-key [?\C-\s-e] 'show-entry) ; this heading's body
	(local-set-key [?\C-\s-i] 'show-children)	; immediate child subs
	(local-set-key [?\C-\s-k] 'show-branches)	; all sub-headings
	(local-set-key [?\C-\s-s] 'show-subtree) ; expand here and below
	;; Moves:
	(local-set-key [?\C-\s-u] 'outline-up-heading)							 ; Up
	(local-set-key [?\C-\s-n] 'outline-next-visible-heading)		 ; Next
	(local-set-key [?\C-\s-p] 'outline-previous-visible-heading) ; Previous
	(local-set-key [?\C-\s-f] 'outline-forward-same-level)			 ; Forward
	(local-set-key [?\C-\s-b] 'outline-backward-same-level)	; Backward
)

;;; SQL Support

(load "jgd-sql")
(load "jgd-sql-connect")

;;; Printing

   ;; (require 'printing)		; load printing package
   ;; (setq pr-path-alist
	 ;;    '((unix      "." "~/bin" ghostview mpage PATH)
	 ;;      (ghostview "$HOME/bin/gsview-dir")
	 ;;      (mpage     "$HOME/bin/mpage-dir")
	 ;;      ))
   ;; (setq pr-txt-name      'dell
	 ;; 	 ps-paper-type 'letter
;;		ps-font-size 6.0
;;		ps-print-header nil
;;		ps-landscape-mode nil
;;		ps-number-of-columns 2
;; 		 )
;;    (setq pr-txt-printer-alist '(
;; 																 (hp "lpr" nil "hp")
;; 																 (dell "lpr" nil "dell")
;; 																 ;;           (prt_07c nil   nil "prt_07c")
;; 																 ))
;;    (setq pr-ps-name       'hp)
;;    (setq pr-ps-printer-alist '(
;; 																(hp "lpr" nil "-P" "hp")
;; 																(dell "lpr" nil "-P" "dell")
;; 																;;           (lps_07c "lpr" nil nil  "lps_07c")
;; 																;;           (lps_08c nil   nil nil  "lps_08c")
;; 																) )
;; (pr-update-menus t)		; update now printer and utility menus


;;; Misc

;; Put the current directory of a shell in the mode-line
(defconst jgd-short-dirpath-max 40)
(defun jgd-short-dirpath (&optional path)
	(let ( (d (replace-regexp-in-string "\\`/[^/]*/[^/]*" "~" (expand-file-name (or path ".")))) (last-d nil) )
		(while (or (> (length d) jgd-short-dirpath-max) (equal d last-d))
			(setq last-d d)
			(setq d (replace-regexp-in-string "\\`[~/.]*[^/]*/?" "..." d)) )
		(replace-regexp-in-string "/?\\'" "" d)	) )
(defun add-mode-line-dirtrack ()
	(add-to-list 'mode-line-buffer-identification
;		'(:propertize (" " (:eval (jgd-short-dirpath)) " ") face dired-directory)
		'(:propertize (:eval (jgd-short-dirpath)) face dired-directory)
		))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)
;;

(jgd-update-union 'auto-mode-alist '("\\.mmd\\'" . markdown-mode) )

(defun do-nothing ()
  "fagedaboutit"
  (interactive)
)

(global-set-key [XF86AudioPrev] 'do-nothing)
(global-set-key [C-XF86AudioPrev] 'do-nothing)

(server-start)

(winner-mode 1)

;; (require 'frame-bufs)
;; (load-file "~/Lib/Emacs/frame-bufs.elc")
;; (frame-bufs-mode t)

(menu-bar-mode 1)              ;  it got disabled somewhere!

;;; Tramp

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(require 'tramp)
(defun sudo-edit-current-file ()
  (interactive)
  (let ((position (point)))
    (find-alternate-file
     (if (file-remote-p (buffer-file-name))
         (let ((vec (tramp-dissect-file-name (buffer-file-name))))
           (tramp-make-tramp-file-name
            "sudo"
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            (tramp-file-name-localname vec)))
       (concat "/sudo:root@localhost:" (buffer-file-name))))
    (goto-char position)))
