;; * NGender Emacs Profile Base -*- lexical-binding: t; -*-
;; Copyright (C) 2018 J. Greg Davidson
;; Author: J. Greg Davidson <jgd@ngender.net>
;; Homepage: https://ngender.org/RPTUG
;; - we need to make sure that /RPTUG exists as an "alias"
;; Package-Requires: ((seq)(map)(cl-lib)(emacs "25.1"))

;;; ** Commentary

;; *** Macros for Power User Code

;; We provide some handy macros for including and
;; configuring Emacs Packages and NGender Modules.
;; No quotes ' or " are required to use these!
;; Most of these are intended for use in your
;; User-Me/init-me.el file.

;; **** Managing NGender Emacs Modules

;; When there's an NGender module for what you want,
;; it should take care of downloading and installing
;; any required packages for you.

;; (ngender module)
;; Require the module named "module"
;; It will be loaded if it hasn't been already

;; (ngender module feature1 feature2 ... )
;; Load module with specified features.
;; See module file for feature documentation.

;; (ngender-forget module)
;; Forget we've ever loaded any such module.
;; Next (ngender module) will load the module again!

;; **** Managing Files of Emacs Lisp Code

;; When you don't have a module, perhaps you have some
;; regular Emacs Lisp code and you just need a place to put
;; it where Emacs will find it

;; (ngender-emacs-path some/dir)
;; Add some/dir as a directory (folder) where you can place
;; regular emacs .el or .elc files.  The directory (you will
;; need to create it) will be placed on the Emacs 'load-path
;; list.  Files placed there can be loaded with the Emacs
;; load function or (if they use provide) the Emacs require
;; function.

;; (ngender-emacs-path-delete some/dir)
;; Remove some/dir from the Emacs 'load-path list

;; **** Managing Emacs Packages and Package Archives

;; Perhaps you want to deal with Emacs Packages and
;; Package Archives yourself?

;; (ngender-archive package-archive ... )
;; Add the specified package-archives to the Emacs 'package-archives list

;; (ngender-package package ... )
;; Install the specified packages from the Emacs archives

;; *** Macros for NGender Module Writers

;; Unless your module is unusually simple you're going to
;; have to learn some Emacs Lisp and some of how Emacs does
;; things, especially how Emacs does things with Packages.
;; Studying the rest of the code of this file may help you
;; learn some of that.  These two macros should help you get
;; started:

;; (ngender-features module)
;; Returns the list of features requested of your module.
;; Will return the empty list for a default configuration.
;; You must give the name of your module.

;; (ngender-provide module)
;; YOU MUST CALL THIS when your module is done!
;; Moves your module name and features from the loading list
;; to the loaded list.  You must give your module name.
;; Note that each feature will itself be a list looking like
;; (feature-symbol feature-arguments ...)

;; *** Room for improvement

;; Better macros would signal a compile time error when
;; called with appropriate arguments.

;;; * Code

;; ** Dependencies

;; Prefer the functionality of the modern packages
;; map and seq over cl-lib when possible!
(require 'seq)
(require 'map)
(require 'cl-lib)

;; ** Warnings, Errors, Validating

(defun ngender-warn (x expected &optional tag level)
	"warn that x was not what we expected"
	(lwarn (or level 'emacs) (or tag :warning) "Expected %s, got %s" expected x) nil )

(defun ngender-validate (x type-test type-name &optional level tag)
	(if (funcall type-test x)	x
		(ngender-warn x type-name (or level '(type)) (or tag :warning)) ) )

(defun ngender-validate-list (list type-test type-name)
	"filter out elements of list which fail type-test expecting them to be type-name things"
	(seq-filter (lambda (x) (ngender-validate x type-test type-name)) list) )

;; ** Define some functions for managing lists, sets, paths

;; *** Filtering Lists

(defun ngender-symbol-value (symbol &optional default)
"fetch list from global or dynamic variable"
(if (boundp symbol) (symbol-value symbol) default) )

(defun ngender-update-filter (symbol predicate)
	"symbol should be sequence; ensure symbol's value is nil, filter it with predicate"
  (set symbol (seq-filter predicate (ngender-symbol-value symbol))) )

;; *** Using Lists As Sets

;; A bag is a list of items
;; A list-set is a bag without duplicates (using equal)

(defun ngender-union-list-of-bags (bags)
  "return list set union of list of bag lists"
	(delete-dups (apply #'append bags)) )

(defun ngender-union-bags (&rest bags)
  "return list set union of multiple bag lists"
	(ngender-union-list-of-bags bags) )

(defun ngender-update-union-with-list-of-bags (symbol list-of-bags)
	"update symbol as set list from current value and bags in list assuming current value is usually biggest"
  (set symbol
		(ngender-union-list-of-bags (append list-of-bags (list (ngender-symbol-value symbol)))) ) )

(defun ngender-update-union-with-bags (symbol &rest bags)
	"update symbol as set list from current value and multiple bags"
  (ngender-update-union-with-list-of-bags symbol bags) )

(defun ngender-update-union-with-items (symbol &rest items)
	"update symbol as set list from current value and multiple items"
  (ngender-update-union-with-bags symbol items) )

;; *** Managing Path Set Lists

(defun ngender-filter-dirs (paths)
	(ngender-validate-list
		(mapcar (lambda (f) (expand-file-name f (ngender-load-dir))) paths)
		#'file-directory-p 'directory ) )

(defun ngender-add-paths (symbol paths do-append)
	"prepend or append candidate directory set with directory set bound to symbol"
	(set symbol (let ( (paths (ngender-symbol-value symbol))
										 (newbies (ngender-filter-dirs paths)) )
								(if do-append
									(ngender-union-bags paths newbies)
									(ngender-union-bags newbies paths) ) )) )

(defun ngender-prepend-paths (symbol &rest dirs)
	"prepend candidate directory paths in front of directory set bound to symbol"
	(ngender-add-paths symbol dirs nil) )

(defun ngender-append-paths (symbol &rest dirs)
	"append candidate directory paths to end of directory set bound to symbol"
	(ngender-add-paths symbol dirs t) )

;; ** Association List Support

;; Emacs 27 has assoc-delete-all but we don't so we'll just have to work harder:

(defun ngender-assoc-delete-all (key alist)
	"replace with assoc-delete-all as soon as emacs27 is in common use!!"
	(seq-filter (lambda (x) (equal key (car x))) alist) )

(defun ngender-move-from-to-match (src-list dst-list match)
	"destructively move first matching element from src to dst; returning dst"
	(let ( (src (ngender-symbol-value src-symbol)) (dst (ngender-symbol-value dst-symbol)) (rest) )
		(cond
			( (null src) dst )					; src is empty, return dst
			( (funcall match (car src))	; first element matches
				(setq rest (cdr src))			; save rest of src list
				(set src-symbol rest)			; splice out matching cell
				(rplaca src dst)					; attach cell with match to dst
				(set dst-symbol src) )		; set and return new dst
			(t (let ( (next (cdr src)) )
					 (while (and next (not (match (car next))))
						 (setq src next)
						 (setq next (cdr next)) )
					 (if (not next)								; nothing matched
						 dst												; so return dst as promised
						 (setq rest (cdr next))			; otherwise, save rest of src list
						 (rplacd src rest)					; splice out matching cell
						 (rplacd next dst)					; attach cell with match to dst
						 (set dst-symbol next) ) ) ) ) ) ) ; set and return new dst
					 
(defun ngender-alist-move-from-to-key (src dst key)
	"destructively move first matching element from src to dst; returning value of dst"
	(ngender-move-from-to-match src dst (lambda (x) (equal key (car x)))) )

;; ** Emacs Package Archive Repository Support

(require 'package)

(defvar *ngender-known-package-archives*
	'( ("gnu" . "https://elpa.gnu.org/packages/")
		 ("marmalade" . "http://marmalade-repo.org/packages/")
		 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
		 ("org" . "https://orgmode.org/elpa/")
		 ) "package archives we know exist" )

(defvar package-archives
	'( ("gnu" . "http://elpa.gnu.org/packages/") )
  "requested package archives" )

;; the regexp patterns are only heuristic, not definitive!
(defun ngender-archive-name-p (name) (and (stringp name) (string-match "^[[:alpha:]][[:alnum:]-]*$" name)))
(defun ngender-archive-url-p (url) (and (stringp url) (string-match "^https?://" url)))

;; (ngender-validate-list
;; 	*ngender-known-package-archives* (lambda (x) (ngender-archive-name-p (car x))) "package archive name" )

;; (ngender-validate-list
;; 	*ngender-known-package-archives* (lambda (x) (ngender-archive-url-p (cdr x))) "package archive url" )

(defun ngender-archive-p (pair)
	"return the cons archive represented by the list or cons archive pair or nil"
	(if (not (consp pair))
		nil
		(let* ( (name (car pair)) (cdr (cdr pair)) (is-list (consp cdr)) (url (if is-list (car cdr) cdr)) )
			(if (not (and (ngender-archive-name-p name) (ngender-archive-url-p url)))
				nil
				(if is-list (cons name url) pair) ) ) ) )

;; (ngender-archive-p '("org" . "https://orgmode.org/elpa/"))
;; (ngender-archive-p '("org" "https://orgmode.org/elpa/"))

(defun ngender-add-package-archive-pair (symbol new-pair)
	(let* ( (key (car new-pair)) (alist (ngender-symbol-value symbol)) (old-pair (assoc key alist)) )
		(if (equal old-pair new-pair) t
			(when old-pair
				(warn "changing url for %s from %s to %s in %s" key (cdr old-pair) (cdr new-pair) symbol)
				(set symbol (ngender-assoc-delete-all key alist)) )
			(set symbol (cons new-pair (ngender-symbol-value symbol))) ) ) )

;; (defvar *ngender-test-package-archive* '())
;; (setq *ngender-test-package-archive* '())
;; (ngender-add-package-archive-pair '*ngender-test-package-archive* '("org" . "https://orgmode.org/elpa/"))
;; *ngender-test-package-archive*

(defun ngender-add-package-archive-by-key (key)
	(let ( (pair (assoc key *ngender-known-package-archives*)) )
		(cond
			( (symbolp key) (ngender-add-package-archive-by-key (symbol-name key)) )
			( (not (stringp key)) (warn "expected string archive key: %s" key) )
			( (null pair) (warn "expected known archive key: %s" key) )
			( t (ngender-add-package-archive-pair 'package-archives pair) ) ) ) )

(defun ngender-package-archive (&rest args)
	"add archives to package-archives and new ones to *ngender-known-package-archives*"
	(dolist (a args)
		(cond
			( (consp a) (let* ( (pair (ngender-archive-p a)) )
										(if (not pair)
											(warn "expected archive: %s" a)
											(ngender-add-package-archive-pair '*ngender-known-package-archives* pair)
											(ngender-add-package-archive-pair 'package-archives pair) ) ) )
			( (or (stringp a) (symbolp a)) (ngender-add-package-archive-by-key a) )
			( t (warn "expected archive name or pair %s" a) ) ) ) )


(defmacro ngender-archive (&rest archives)
	(cons
		'ngender-package-archive
		(mapcar (lambda (x) (cond
													( (symbolp x) (symbol-name x) )
													( (stringp x) x )
													( t (list 'quote x) ) )) archives) ) )

;; (macroexpand-1 '(ngender-archive gnu "marmalade" ("org" . "https://orgmode.org/elpa/")))
												

(defun ngender-drop-assoc-by-key (symbol key)
  (set symbol (ngender-assoc-delete-all key (ngender-symbol-value symbol)))
)

(defun ngender-package-archive-delete (key)
	(ngender-drop-assoc-by-key 'package-archives key)
)

(defun ngender-package-archive-forget (key)
	(ngender-drop-assoc-by-key 'package-archives key)
	(ngender-drop-assoc-by-key '*ngender-known-package-archives* key)
)

;; Some notes on how Emacs manages package archives:
;; Variables:
;; package-archives - where to fetch packages from
;; package-load-list - which installed packages to load - might be (all)
;; package-archive-contents - Cache of the contents of the
;;		Emacs Lisp Package Archive.  This is an alist mapping
;;		package names (symbols) to non-empty lists of
;;		`package-desc' structures.
;; Functions:
;; package-initialize - load packages from package-load-list
;; package-refresh-contents - download descriptions of all packages on package-archives

;; Various package files will use this to ensure
;; that their required packages are loaded.
;; How could we fail gracefully?? This will not!!
(defun ngender-package-function (&rest packages)
	"ensure these packages are loaded"
	(dolist (p packages)
		(unless (package-installed-p p)
			(package-install p) ) ) )

(defmacro ngender-package (&rest packages)
	(cons 'ngender-package-function (mapcar #'ngender-a-string packages)) )

;; (macroexpand-1 '(ngender-package org))

;; ** File and Directory Support

(defun ngender-load-dir ()
	"the directory of the file calling for a load or the current directory"
	(if load-file-name
		(or (file-name-directory load-file-name) ".")
		default-directory ) )

(defun require-file-path (name type-test type-name &optional parent)
	(let (( path (expand-file-name name (if parent parent *ngender-emacs-home*)) ))
		(let (( p (file-chase-links path) ))
			(if (funcall type-test p) path
				(ngender-warn p type-name '(init)) ) ) ) )

(defun require-file (path &optional parent)
	(require-file-path path #'file-regular-p 'file parent) )
(defun require-dir (path &optional parent)
	(require-file-path path #'file-directory-p 'directory parent) )

(defun ngender-a-string (x)
	"given a string, a symbol or a quoted string or symbol, just provide a string - handy for macros"
	(cond
		( (stringp x) x )
		( (symbolp x) (symbol-name x) )
		( (and (consp x) (eq 'quote (car x)))
			(ngender-a-string (cadr x)) ) ) )

;; (mapcar #'ngender-a-string
;;  '("foo" foo 'foo) )

;; ** Fancy Emacs Load-Path Support

;; This whole section needs to be redesigned and reworked!!
;; We need to keep module directories off of the load-path
;; We need to manage modules and module features separately!

;; The *ngender-load-path* will be kept ordered as follows, first to last:
;; (1) User Subdirectories
;; (2) Group (Project) Directories
;; (3) *ngender-home*

;; A handy macro will allow adding package directories to the Emacs load-path
;; Another handy macro will allow conveniently dropping such

(defvar *ngender-load-path*	(list *ngender-home*) "where to find ngender modules")

(defvar *ngender-user-dirs* '() "normally just directory User-Me")
(defvar *ngender-group-dirs* '()  "possible shared group module directories")
(defconst *ngender-core-dirs* (list *ngender-home*) "should not change!")
(defconst *ngender-path-lists*
	'(*ngender-user-dirs* *ngender-group-dirs* *ngender-core-dirs*)
"*ngender-load-path* will be reconstructed from these sublists, deduped, left-to-right" )

(defun ngender-rebuild-load-path ()
  "rebuild *ngender-load-path* with elements of lists named in
*ngender-path-lists* appearing in front in order"
  (setq *ngender-load-path* (delete-dups
	 (apply #'append (mapcar #'symbol-value *ngender-path-lists*)))) )

;; (ngender-rebuild-load-path)

(defun ngender-path-dirs (symbol paths)
	"add directory paths to the front of the list named by
symbol and rebuild *ngender--load-path-"
	(set symbol (ngender-filter-dirs paths))
	(ngender-rebuild-load-path) )

(defun ngender-path-delete (symbol path)
	"delete paths from the list named by symbol and rebuild *ngender-load-path*"
	(set symbol (delete path (ngender-symbol-value symbol)))
	(ngender-rebuild-load-path) )

(defun ngender-user-dir (&rest paths)
	"add paths to user subdirectories and rebuild *ngender-load-path*"
	(ngender-path-dirs '*ngender-user-dirs* paths)
)

(defun ngender-user-dir-delete (path)
	"delete path from user dirs and rebuild *ngender-load-path*"
	(ngender-path-delete '*ngender-user-dirs* path)
)

(defun ngender-group-dir (&rest paths)
	"add paths to group dirs and rebuild *ngender-load-path*"
	(ngender-path-dirs '*ngender-user-dirs* paths) )

(defun ngender-group-dir-delete (path)
	"delete path from group dirs and rebuild *ngender-load-path*"
	(ngender-path-delete '*ngender-group-dirs* path) )

(defmacro ngender-emacs-path (&rest paths)
	"ensure paths on emacs load-path"
	`(ngender-update-union-with-bags 'load-path ',(mapcar #'ngender-a-string paths)) )

;; (macroexpand-1 '(ngender-emacs-path some/dir))

(defmacro ngender-emacs-path-delete (path)
	"delete path from emacs load-path"
	`(ngender-path-delete 'load-path ,(ngender-a-string path)) )

;; (macroexpand-1 '(ngender-emacs-path-delete some/dir))

;; ** Window Management Functions

;; Assumes an X-Window System Window Manager!!
;; Why does this toggle?
(defun ngender-toggle-fullscreen ()
	"Ask the X Window Manager to toggle to/from FullScreen Mode"
	(interactive)
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
		'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0) )
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
		'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0) )
)
(global-set-key [?\s-f] 'ngender-toggle-fullscreen)

(defun ngender-split-window (n)
	(interactive "p")
	(if (and (= n 1) (boundp 'ngender-num-cols))
		(setq n ngender-num-cols) )
	(ngender-toggle-fullscreen)
	(cond
		(	(> n 1)
			(split-window-horizontally)
			(balance-windows)
			(ngender-split-window (- n 1))
		)
		(	(< n -1)
			(split-window-vertically)
			(balance-windows)
			(ngender-split-window (- n 1))
		)
)	)
(global-set-key [?\s-m] 'ngender-split-window)

;; Are we running under a window-system?
;; The variable window-system will be nil if not,
;; otherwise it will tell us which gui we're using!
(defun using-gui-p () (and (boundp 'window-system) window-system))

;; do we want subminor??
;; should we burst minor-version on "."?? and have &rest minors??
(defun version>= (major &optional minor)
	(or (> emacs-major-version major)
		(and (= emacs-major-version major)
			(or (null minor)
				(>= emacs-minor-version minor) ) ) ) )

;; ** Font Issues

;; Variable or Fixed Pitch Mode for Source Code

;; ngender-pitch-mode is called by many mode-hook functions!
;; If you generally prefer variable-pitch-mode, then
;; (defvar *variable-pitch-mode-value* :variable)
;; otherwise just leave it unbound!
;; This could be developed further!!

(defun ngender-pitch-mode ()
"set variable-pitch-mode to *ngender-pitch-mode* if bound"
	(if (boundp '*ngender-pitch-mode*)
		(setq *variable-pitch-mode*
			(cond
				((eq :variable *ngender-pitch-mode*) 1)
				((eq :fixed *ngender-pitch-mode*) 0)
				(t *ngender-pitch-mode*) ) ) ) )

;; The prefix argument specifies the font size, default 14.
;; Should it warn or error if *ngender-frame-font* isn't set??
(defun ngender-frame-font (n)
  "set frame font according to *ngender-frame-font* if bound"
  (interactive "p")
	(if (boundp '*ngender-frame-font*)
		(let (
					 (nn (if (< n 4) 8 n))
					 (f (string-match-p "^[^%]*%d[^%]*$" *ngender-frame-font*)) )
			(set-frame-font
				(cond
					(f (format *ngender-frame-font* nn))
					(t (format "%s-%d"  *ngender-frame-font* nn)) ) ) ) ) )

;;; Define some interactive functions

(defun show-interactive-p (n)
  "show interactive p"
  (interactive "p")
  (message "%d" n) )

(defun show-interactive-P (n)
  "show interactive P"
  (interactive "P")
  (message "%s" n) )

(defun show-interactive-n (n)
  "show interactive n"
  (interactive "nHow many: ")
  (message "%d" n) )

(defun show-interactive-N (n)
  "show interactive N"
  (interactive "NHow many: ")
  (message "%d" n) )

;; The standard command "compile" defaults to running "make -k".
;; Define "make" to do the same thing but without the "-k".
;; Darn - it is not working the same way.  Fix it, and when it
;; works, use it instead of compile in the keybinding below.
;; Alas, this doesn't work!!!
(defun make ()
  (interactive)
	(let ( (compile-command (purecopy "make")) )
		(compile "make") ) )

;; - Hold down Meta (aka Alt) and type a digit to select the
;;   desired format before running this function.
;; - The day of the week will be blank padded, which will
;;   be weird when the day of the week is 1-9.  This should
;;   be fixed!!!
;; - It would be cool to rationalize these features into a
;;   bitset interpretation of the arguments!
(defun ngender-time (n)
	"insert date (and optionally the time) in a nice format according to the prefix argument"
  (interactive "p")
  (insert (format-time-string (cond ( (= n 0) "%e %B %Y" )
                                    ( (= n 1) "%A %e %B %Y" )
                                    ( (= n 2) "%A, %e %B %Y" )
                                    ( (= n 3) "%l:%M%p %A %e %B %Y" )
                                    ( (= n 4) "%Y-%m-%d %T%z %A" )
                                    ))) )
;; - Invoke with Super-t, i.e. "Windows Button" + t
(global-set-key [?\s-t] 'ngender-time)

;; What shall I bind this to?
(defun ngender-next-window (n)
  (interactive "p")
  (other-window n)
)

;; What shall I bind this to?
(defun ngender-previous-window (n)
  (interactive "p")
  (other-window (- n))
)

;; ** Tramp

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


;; should we warn if argument has an extension??
(defun ngender-normalize-module (x)
	"a module name is a filename string w/o extensions; we allow some substitutes"
	(ngender-a-string x) )

(defun ngender-normalize-feature (x)
	"a feature is a list whose first element is a symbol; we allow some substitutes"
	(let ( (warning (lambda (x) (ngender-warn x "feature"))) )
		(cond
			( (consp x)
				(let ( (head (car x)) )
					(cond
						( (eq 'quote head) (ngender-normalize-feature (cadr x)) )
						( (symbolp head) x )
						( (stringp head) (cons (intern head) (cdr x)) )
						( t (funcall warning x) nil ) ) ) )
			( (symbolp x) (list x) )
			( (stringp x) (funcall warning x) (list (intern x)) )
			( t (funcall warning x) nil) ) ) )

;; (mapcar #'ngender-normalize-feature
;;  '(foo "foo" 'foo ("foo" bar) '("foo" bar)) )

;; ** ngender require, load, provide with macros

(defvar *ngender-modules-loaded* '() "assoc list of (module features...) already loaded")

(defvar *ngender-modules-loading* '() "assoc stack of (module features...) being loaded")

(defun ngender-forget-loading (module)
	"remove all instances of given module from list of modules in process of loading"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(setq *ngender-modules-loading* (ngender-assoc-delete-all module *ngender-modules-loading*) ) ) ) )

(defun ngender-forget-loaded (module)
	"remove all instances of given module from list of modules already loaded"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(setq *ngender-modules-loaded* (ngender-assoc-delete-all module *ngender-modules-loaded*)) ) ) )

(defun ngender-loading (module)
	"return the (module feature...) list requested for the given module being loaded"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(assoc module *ngender-modules-loading*) ) ) )

(defmacro ngender-features (module)
	"provide just the feature list, leaving off the module name"
	`(cdr (ngender-loading ,(ngender-normalize-module module))) )

;; (macroexpand-1 '(ngender-features foo))
		
(defun ngender-provide-function (module)
	"move (module feature...) from loading to loaded - used by macro ngender-provide"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(let ( (module+features (assoc module *ngender-modules-loading*)) )
				(if (not module+features)
					(warn "ngender-provide-function: no %s loading, doing nothing" module)
					(ngender-alist-move-from-to-key '*ngender-modules-loading* '*ngender-modules-loaded* module) ) ) ) ) )

;; file a bug with emacs in re the apparent dynamic binding
;; of free / global variables with let.  There needs to be
;; a clear way to do this so we won't be hit by software rot!!
(defun ngender-load-module (module &optional NOERROR NOMESSAGE)
	"load the specified module on the *ngender-load-path*"
	(let ( (load-path *ngender-load-path*) ) ; dynamic binding of global variable!!
		(load module NOERROR NOMESSAGE) ) )

(defun ngender-load (module &rest features)
	"show we are loading (module features...) and load module using our load path"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(ngender-warn module-in "module")
			(let ( (features (seq-filter #'consp (mapcar #'ngender-normalize-feature features))) )
				(let ( (module+features (cons module features)) )
					(setq *ngender-modules-loading* (cons module+features *ngender-modules-loading*))
					(ngender-load-module module) ) ) ) ) ) ; module can pull features off loading list

(defun ngender-require (module &rest features)
	"load module with required features unless already done - used by macro ngender - order of features matters"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(let ( (features (seq-filter #'consp (mapcar #'ngender-normalize-feature features))) )
				(unless (member (cons module features) *ngender-modules-loaded*)
					(apply #'ngender-load (cons module features)) ) ) ) ) )

(defmacro ngender (&rest module+features)
	(cons 'ngender-require (mapcar (lambda (x) (list 'quote x)) module+features)) )

;; (macroexpand-1 '(ngender foo))
;; (macroexpand-1 '(ngender foo this that))

(defmacro ngender-provide (module)
	(list 'ngender-provide-function (ngender-normalize-module module)) )

;; (macroexpand-1 '(ngender-provide foo))

(defun ngender-forget-function (module)
	"fagedaboutit"
	(let ( (module (ngender-normalize-module module)) )
		(when module
			(ngender-forget-loading ,module)
			(ngender-forget-loaded ,module) ) ) )

(defmacro ngender-forget (module)
	(list 'ngender-forget-function module) )

;; (macroexpand-1 '(ngender-forget foo))

;; ** Provide

(add-to-list '*ngender-modules-loaded* (list "ngender"))
