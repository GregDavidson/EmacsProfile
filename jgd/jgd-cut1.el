; ~/.emacs.d/jgd.el - some emacs extensions
; J. Greg Davidson

; Settings and keybindings are at the bottom of this file;
; you'll want to look there unless you're interested in
; emacs lisp programming.

; Table of Contents
;	Table of Contents
;	Dependencies - provide and require
;	Define some functions for managing lists, sets, paths
;	Customize some directory path lists
;	Define some more functions
;	Define some interactive functions
;	Mode-Specific Customizations
;		Tcl Preferences
;		C/C++ Preferences
;		etc.
;	Key Bindings - (Re)Define some keyboard shortcuts
;	Set some miscellaneous variables

; Dependencies - provide and require

(provide 'jgd)

;; (require 'x-fonts)
(require 'jit-lock)

(require 'rx-jgd)

; Overall Preferences

;; Indenting by tabs
; Problem: We want to be able to change the tab-width
; but we want all indenting to be done in tabs.  There
; seems to be an interaction as well as clumsiness here!

(defconst jgd-min-indent 1)
(defconst jgd-default-indent 2)
(defconst jgd-default-tab-width 2)

; Define some functions for managing lists, sets, paths

;; functions for filtering lists

(defun jgd-filter-reverse (items predicate)
  "reversed sublist of items which satisfy predicate"
  (let (results)
    (dolist (item items results)
      (or (apply predicate item) (setq results (cons item results))) ) ) )

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
      (or (member item bag) (setq bag (cons item bag))) ) )

(defun jgd-update-union (symbol &rest items)
  (set symbol (jgd-union-list (symbol-value symbol) items)) )


;; functions for managing paths

(defun jgd-union-dirs (dirs new-dirs)
  "union of dirs and new-dirs"
  (dolist (dir new-dirs dirs)
      (or (member dir dirs)
	  (not (file-directory-p dir))
	  (setq dirs (cons (expand-file-name dir) dirs)) ) ) )

(defun jgd-update-dirs (symbol &rest dirs)
	(set symbol
	     (jgd-union-dirs
	      (if (boundp symbol) (symbol-value symbol) '())
	      dirs) ) )

(defun jgd-union-files (files new-files)
  "union of files and new-files"
  (dolist (file new-files files)
      (or (member file files)
	  (not (file-readable-p file))
	  (setq files (cons (expand-file-name file) files)) ) ) )

(defun jgd-update-files (symbol &rest files)
  (set symbol (jgd-union-files (symbol-value symbol) files)) )

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

; Customize some directory path lists

;; (jgd-update-dirs (if (boundp 'Info-directory-list)
;; 		     'Info-directory-list 'Info-default-directory-list)
(jgd-update-dirs 'Info-directory-list
              "/usr/share/info/"
              "/usr/share/texmf/doc/info/"
;;              "/usr/local/mercury-0.11.1-beta-2004-06-30/info"
;;              "/opt/gnome/share/info"
              )

(jgd-update-dirs 'load-path
              "~/.emacs.d/"
              "~/Lib/Emacs/"
              "~/Lib/Emacs/Scala/"
              "/data/greg/Lib/Emacs/lisp"
;;              "/usr/local/Mercury/lib/mercury/elisp"
;;              "/usr/local/src/flora2/emacs"
;;              "/usr/local/src/XSB/etc"
;;              "/usr/share/doc/ruby-1.6.4/misc"
              )

; Define some more functions

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

;; Associate a decent font with the current frame.
;; The prefix argument specifies the font size, default 14.
(defun jgd-frame-font (n)
  "Set a readable non-proportional frame font."
  (interactive "p")
  (set-frame-font
   (format
    "-adobe-courier-medium-r-normal--%d-*-*-*-m-*-iso8859-1"
    (if (< n 4) 14 n) )
   't )
)

; Define some interactive functions

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
(defun make (n)
  (interactive)
  (compile "make") )

;; Insert the date (and optionally the time) in a nice format.
;; - Invoke with Super-t, i.e. "Windows Button" + t
;; - Hold down Meta (aka Alt) and type a digit to select the
;;   desired format before running this function.
;; - The day of the week will be blank padded, which will
;;   be wierd when the day of the week is 1-9.  This should
;;   be fixed!
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

(defun jgd-next-window (n)
  (interactive "p")
  (other-window n)
)

(defun jgd-previous-window (n)
  (interactive "p")
  (other-window (- n))
)

; Tcl Preferences

(require 'tcl)

(defun jgd-tcl-preferences ()
  (setq tcl-application "tclsh"
	tcl-indent-level jgd-default-indent
	tcl-continued-indent-level jgd-default-indent
	tcl-use-smart-word-finder t)  )

; C/C++ Preferences

(require 'cc-mode)

(defconst jgd-c-style
  `((c-tab-always-indent . t)
    (c-comment-only-line-offset . ,jgd-default-indent) ; was 4
    (c-hanging-braces-alist . ((substatement-open after)
			       (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after)))
    (c-cleanup-list . (scope-operator
		       empty-defun-braces
		       defun-close-semi))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(case-label        . ,jgd-default-indent) ; was 4
			(block-open        . 0)
			(knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    (truncate-partial-width-windows . nil)
    )
  "C Programming Style for Virtual Infinity Systems")

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
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
)

; Prolog preferences

;; see http://turing.ubishops.ca/home/bruda/emacs-prolog/
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(defvar prolog-system 'swi)
(jgd-update-union 'auto-mode-alist
		  '("\\.pl$" . prolog-mode)
		  '("\\.m$" . mercury-mode) )

; Mozart/Oz preferences

(when (not (boundp 'ozhome))
  (defvar ozhome (or (getenv "OZHOME") "/usr/lib/mozart") )
  (defvar oz-bin (concat ozhome "/bin"))
  (defvar oz-elisp (concat ozhome "/share/elisp"))
  (when (file-directory-p ozhome)
    (when (and (file-directory-p oz-bin)
	       (not (member exec-path oz-bin)) )
      (nconc exec-path (list oz-bin))
      (setenv "PATH" (concat (getenv "PATH") ":" oz-bin)) )
  (jgd-update-dirs 'load-path oz-elisp)
  
  (jgd-update-union 'auto-mode-alist
		    '("\\.oz\\'" . oz-mode)
		    '("\\.ozg\\'" . oz-gump-mode) )
  
  (autoload 'run-oz "oz" "" t)
  (autoload 'oz-mode "oz" "" t)
  (autoload 'oz-gump-mode "oz" "" t)
  (autoload 'oz-new-buffer "oz" "" t) ) )

; Haskell preferences - for 2.7.0, November 2009

(jgd-update-dirs 'load-path "~/Lib/Emacs/HaskellMode")

(load "haskell-site-file")

;; (jgd-update-union 'auto-mode-alist
;; 		  '("\\.[hg]s$"  . haskell-mode)
;; 		  '("\\.hi$"     . haskell-mode)
;; 		  '("\\.l[hg]s$" . literate-haskell-mode) )

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

; adding any of the following lines according to
; which modules you want to use:

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)

; CLisp Common Lisp Preferences

(defun slime-setup-jgd (slime-dir lisp-program)
  "Set up Slime to use the right Common Lisp"
  (jgd-update-dirs 'load-path slime-dir)
  (when (and (member slime-dir load-path)
	     (file-exists-p lisp-program) )
    (setq inferior-lisp-program lisp-program)
    (require 'slime)
    (slime-setup) ) )

(defun slime-setup-clisp ()
  (interactive)
  (slime-setup-jgd "~/Lib/Emacs/slime/" "/usr/bin/clisp") )

;;; Clojure Preferences ;; broken!

; (jgd-update-dirs 'load-path "~/Lib/Emacs/clojure-mode/")
; (require 'clojure-auto)

;; (defun jgd-clojure-mode-inits ()
;;   "Customize clojure-mode as I like it"
;;   (interactive)
;; ;  (setq inferior-lisp-program "clojure") ; buffer-local var better?
;;   (setq inferior-lisp-program "~/Lib/JVM/Clojure/play-repl") ; buffer-local var better?
;; )

; (add-hook 'clojure-mode-hook 'jgd-clojure-mode-inits)
;(setq inferior-lisp-program "clojure")

; Key Bindings - (Re)Define some keyboard shortcuts

(global-set-key "\C-cv" 'variable-pitch-mode)

; ignore keysyms generated by my flakey Dell Studio
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
(global-set-key [C-f7] 'blink-matching-open)	; Show matching parenthesis
(global-set-key [C-f8] 'start-kbd-macro)	; start recording commands
(global-set-key [C-f9] 'end-kbd-macro)	; finish and call it a macro
(global-set-key [C-f10] 'call-last-kbd-macro)	; re-execute the macro
; (global-set-key [M-f11] 'upcase-word)
(global-set-key [C-f12] 'repeat-complex-command)

; Set some miscellaneous variables
(setq visible-bell t)
(setq tab-stop-list nil)
(setq inhibit-startup-screen t)

; Require some things in ~/Lib/Emacs

;;; (require 'multi-term) ;;; broken!

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

(defun sql-startup ()
  "Start a sqli session and set sql-buffer"
  (interactive)
  (sql-product-interactive)
  (sql-set-sqli-buffer-generally)
  )

(global-set-key [?\s-s ?\s-q ?\s-l] 'sql-startup )

(defun sql-just-set-the-f*ing-buffer ()
  "Set the SQLi buffer SQL strings are sent to."
  (interactive)
  (let ((default-buffer (sql-find-sqli-buffer)))
    (if (null default-buffer)
	(message "There is no suitable SQLi buffer")
      (setq sql-buffer default-buffer) )
					;(run-hooks 'sql-set-sqli-hook)
) )


(defun jgd-outline-minor-map ()
	"Create short outline-minor-mode bindings using Ctrl-Super- instead of Ctrl-C @."
	(interactive)
	(local-set-key [?\C-\s-q] 'hide-sublevels) ; Hide everything but the top-level headings
	(local-set-key [?\C-\s-t] 'hide-body)	; Hide everything but headings (all body lines)
	(local-set-key [?\C-\s-o] 'hide-other) ; Hide other branches
	(local-set-key [?\C-\s-c] 'hide-entry) ; Hide this entry's body
	(local-set-key [?\C-\s-l] 'hide-leaves)	; Hide body lines in this entry and sub-entries
	(local-set-key [?\C-\s-d] 'hide-subtree) ; Hide everything in this entry and sub-entries
																				; SHOW
	(local-set-key [?\C-\s-a] 'show-all)	; Show (expand) everything
	(local-set-key [?\C-\s-e] 'show-entry) ; Show this heading's body
	(local-set-key [?\C-\s-i] 'show-children)	; Show this heading's immediate child sub-headings
	(local-set-key [?\C-\s-k] 'show-branches)	; Show all sub-headings under this heading
	(local-set-key [?\C-\s-s] 'show-subtree) ; Show (expand) everything in this heading & below
																				; MOVE
	(local-set-key [?\C-\s-u] 'outline-up-heading)							 ; Up
	(local-set-key [?\C-\s-n] 'outline-next-visible-heading)		 ; Next
	(local-set-key [?\C-\s-p] 'outline-previous-visible-heading) ; Previous
	(local-set-key [?\C-\s-f] 'outline-forward-same-level) ; Forward - same level
	(local-set-key [?\C-\s-b] 'outline-backward-same-level)	; Backward - same level
)

(defun sql-outline-minor-mode ()
	"Add outline-minor-mode to sql-mode."
	(interactive)
	(setq outline-minor-mode-prefix "\C-c\C-o")
	(outline-minor-mode 1)
	(setq outline-regexp "-- [*\f]+")
	(jgd-outline-minor-map)
)

(add-hook 'sql-mode-hook 'sql-outline-minor-mode)
(add-hook 'sql-mode-hook 'sql-just-set-the-f*ing-buffer)
(add-hook 'sql-mode-hook 'jgd-set-default-tab-width)

(defun do-nothing ()
  "fagedaboutit"
  (interactive)
)

(global-set-key [XF86AudioPrev] 'do-nothing)

(defun def-rx-1 (name &rest defs)
	(or (assq name rx-constituents)
		(let ( (new-def (cons name (apply 'concat defs))) )
			(setq rx-constituents (cons new-def rx-constituents))
			new-def) ) )

(defmacro def-rx (name &rest defs)
	`(def-rx-1 ',name (rx ,@defs)) )

(def-rx int (opt (any "+-"))(+ digit))
(def-rx hs space)									; hs = horizontal space
(def-rx hs* (* hs))			; hs* = any (0 or more chars of) hs
(def-rx hs*w hs* word-start)				; hs*w = hs* then a word
(def-rx whs* word-end hs*)	 ; whs* = a word then hs*
(def-rx whs*w whs* word-start)			; whs*w = hs* between words
(def-rx hs+ (+ hs))							; hs+ = some (1 or more chars of) hs
(def-rx s (any space "\n"))							; s = space (perhaps a newline)
(def-rx s* (* s))						; s* = any (0 or more chars of) s
(def-rx s*w s* word-start)		; s*w = s then a word
(def-rx ws* word-end s*)	 ; ws* = a word then s*
(def-rx ws*w ws* word-start)		; ws*w = s* between words
(def-rx s+ (+ s))						; some (1 or more chars of) s
(def-rx name (char alpha "_") (* (char alnum "_"))) ; no schema-qualifier
(def-rx sql-name (opt name ".") name)		; possible schema-qualifier
(def-rx sql-type sql-name (opt "[]")) ; type name (may end in [] for array)
(def-rx not-rpar (not-char ")"))								; any char not a ")"
(def-rx sql-create-			; beginning of an SQL CREATE of something or other
				 point hs*w "CREATE" (opt s+ "OR" s+ "REPLACE") s+)
(def-rx sql-create-function-				 ; beginning of an SQL CREATE FUNCTION
				 sql-create-	"FUNCTION" s+)
(def-rx sql-create-function				; a complete SQL CREATE FUNCTION command
	sql-create-function-
	(group sql-name)																	 ; the function name = \1
	s* "(" s* (group (*? not-rpar)) s* ")" ; the function parameters = \2
	s* (group (*? (not-char "$")))				 ; pre-body clauses = \3
	s*w "AS" ws*													 ;  AS introduces the body
	(group "$" (* (not-char "$")) "$")	; open super-parenthesis = \4
	(group (*? anything))										; the function body = \5
	(backref 4)													; close super-parenthesis = \4
	s* (group (*? (not-char ";"))) s*	; the rest of the clauses = \6
	";" )

;; * sql editing support

;; ** the helper functions

(defun list-interpose-accum (lst delim accum)
	 "helper function for list-interpose"
	 (if (null lst)	(nreverse accum)
			 (list-interpose-accum
				(cdr lst) delim (cons (car lst) (cons delim accum)) ) ) )

(defun list-interpose (lst delim)
	 "the elements of lst interposed with delim"
	 (if (atom lst)	lst
			 (list-interpose-accum (cdr lst) delim (list (car lst))) ) )

(defun list-concat (lst)
	 "concatenate a list of strings"
	 (apply 'concat lst) )

(defun str-drop-word (str word)
	"drop given word and adjacent space"
	(let
		( (delim (concat "[[:space:]\n]*\\<" (regexp-quote word) "\\>[[:space:]\n]*")) )
		(list-concat (list-interpose (split-string str delim t) " "))
) )

(defun sql-args-list (n)
	 (if (zerop n)
			 nil
			 (cons (format "$%d" n) (sql-args-list (1- n))) )
)

(defun sql-args (params-str)
	"return a string of $1,$2,... for params-str"
	(if (string-match (rx bos (* (any " \t\n")) eos) params-str)
		""
		(let	( (n (length (split-string params-str (rx (* (any " \t\n") "," (* (any " \t\n")))) t))) )
			(list-concat (list-interpose (reverse (sql-args-list n)) ","))
) ) )

(defun sql-params (params-str)
	"return a string of $1,$2,... for params-str"
	(if (string-match (rx bos s* eos) params-str)
		""
		(let	( (n (length (split-string params-str (rx s* "," s*) t))) )
			(list-concat (list-interpose (reverse (sql-args-list n)) ","))
) ) )

(defun sql-param-type (param)
		"return the type name given an sql param description"
		(let*
			( (default (rx s* (or "=" (sequence bow "DEFAULT" eow)) s*))
				(split1 (split-string param default))
				(split2 (split-string (nth 0 split1) (rx s+) t)) )
			(nth (1- (length split2) ) split2)
) )

(defun sql-params-types (params-str)
	"return a string of  param1-type,param2-type,... for params-str"
	(if (string-match (rx bos s+ eos) params-str)
		""
		(let ( (split (split-string params-str (rx s* "," s*) t)) )
			(list-concat	(list-interpose (mapcar 'sql-param-type split) ","))
) ) )

(defun nth-match (n &optional matches)
	(let ( (nn (* 2 n)) (m (or matches (match-data))) )
			 ;; Can use either buffer-substring or buffer-substring-no-properties
			 (buffer-substring-no-properties (nth nn m) (nth (1+ nn) m))
) )

(defun all-matches (match-data &optional accum)
	(if (< (length match-data) 2)
		(reverse accum)
		(all-matches (cddr match-data) (cons (nth-match 0 match-data) accum)) ) )

;; ** the interactive functions

(defun beginning-of-sql-function ()
	(interactive)
	(beginning-of-line)
	(while (not (or (bobp) (looking-at (rx sql-create-function-)))) (previous-line))
)

(defun look-at-sql-func ()
	(interactive)
	(beginning-of-sql-function)
	(if (looking-at (rx sql-create-function))
		(match-data) ) )

(defun fork-sql-func-for-try ()
	"Given an sql function definition conforming to the syntax
	described above in (def-rx sql-create-function ...):
	(1) split it into two,
	(2a) give the first version a 'try_' prefix,
	(2b) make sure	that it is 'STRICT'.
  (3a) replace the body of the second copy with a call to the
	first function wrapped in non_null,
	(3b) make sure that it is NOT 'STRICT'."
	(interactive)
		(save-excursion
			(let* ( (case-fold-search t) (matches (look-at-sql-func)) )
				(if matches (let*
					( (intro "CREATE OR REPLACE\nFUNCTION ")
						(name (nth-match 1 matches))
						(params-str (nth-match 2 matches))
						(pre-body-str (nth-match 3 matches))
						(super-quote (nth-match 4 matches))
						(head (concat name "(" params-str ") " pre-body-str " AS " super-quote))
						(body (nth-match 5 matches))
						(tail (str-drop-word (nth-match 6 matches) "STRICT"))
						(in1 "\n\t")
						(in2 (concat in1 "\t")) )
					(insert
						;; (format "%S\n" matches)
						;; (format "%s: %s\n" 'name name)
						;; (format "%s: %s\n" 'params-str params-str)
						;; (format "%s: %s\n" 'pre-body-str pre-body-str)
						;; (format "%s: %s\n" 'super-quote super-quote)
						;; (format "%s: %s\n" 'head head)
						;; (format "%s: %s\n" 'body body)
						;; (format "%s: %s\n" 'tail tail)
						;; "\n"
						intro "try_" head body super-quote " " tail " STRICT;"
						"\n\n"
						intro head
						in1 "SELECT non-null("
						in2 "try_"	name "(" (sql-args params-str) "),"
						in2 "'" name "(" (sql-params-types params-str) ")'"
						in1 ")"
						"\n" super-quote " " tail ";" "\n\n"
) )) ) ) )
