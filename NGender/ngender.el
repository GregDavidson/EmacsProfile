;; * NGender Emacs Profile Base -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; Yikes: We lost our macros:
;; ngender-defvar-list
;; ngender-defmacro-quote-args
;; -- did we lose anything else?

;; ** Dependencies

;; (require 'jit-lock)	; why?
(require 'cl-lib)

;; ** Warnings, Errors, Validating

;; what should the "level" actually be??
(defun ngender-warn (x type &optional level tag)
	(lwarn (or level '(type)) (or tag :warning) "Expected %s, got %s" type x) nil )

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
	"update symbol as set list from current value and bags in list"
  (set symbol
		(ngender-union-list-of-bags (cons (ngender-symbol-value symbol) list-of-bags)) ) )

(defun ngender-update-union-with-bags (symbol &rest bags)
	"update symbol as set list from current value and multiple bags"
  (ngender-update-union-with-list-of-bags symbol bags) )

(defun ngender-update-union-with-items (symbol &rest items)
	"update symbol as set list from current value and multiple items"
  (ngender-update-union-with-bags symbol items) )

;; *** Managing Path Set Lists

(defun ngender-filter-dirs (paths) (ngender-validate-list (mapcar 'expand-file-name paths) 'file-directory-p 'directory))

(defun ngender-add-paths (symbol paths do-append)
  "prepend or append candidate directory set with directory set bound to symbol"
  (set symbol (let ( (paths (ngender-symbol-value symbol))
		     (newbies (ngender-filter-dirs paths)) )
		(if do-append
		    (ngender-union-bags paths newbies)
		  (ngender-union-bags newbies paths) ) )) )

(defun ngender-prepend-paths (symbol &rest dirs)
  "prepend candidate directory paths in front of directory set bound to symbol"
	(funcall #'ngender-add-paths symbol dirs nil) )

(defun ngender-append-paths (symbol &rest dirs)
  "append candidate directory paths to end of directory set bound to symbol"
	(funcall #'ngender-add-paths symbol dirs t) )

;; unused!!
(defun ngender-str-member (str item &optional delim)
  "is item a member of delimited string?"
  (let ( (d (or delim ":")) )
    (string-match (concat ".*" delim (regexp-quote item) delim ".*")
		  (concat delim str delim) ) ) )

;; unused!! except in next function
(defun ngender-str-append (str item &optional delim)
  "append item onto delimited string"
  (concat str (or delim ":") item) )

;; unused!!
(defun ngender-update-str-append (symbol item &optional delim)
  (set symbol (ngender-str-append (symbol-value symbol) item delim)) )

;; unused!!
(defun ngender-exec-path-from-shell ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)) ) )

;; ** Emacs Repository Support

(require 'package)

(defvar *ngender-known-package-archives*
	'( ("gnu" . "http://elpa.gnu.org/packages/")
		 ("marmalade" . "http://marmalade-repo.org/packages/")
		 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
		 ("org" . "https://orgmode.org/elpa/")
		 ) "package archives known to exist" )

(defvar package-archives
	'( ("gnu" . "http://elpa.gnu.org/packages/") )
  "requested package archives" )

;; improve these with regexp matching!!
(defun ngender-archive-name-p (name) (stringp name))
(defun ngender-archive-url-p (url) (stringp url))

(defun ngender-archive-p (pair)
	"return the cons archive represented by the list or cons archive pair or nil"
	(if (not (consp pair))
		nil
		(let* ( (name (car pair)) (cdr (cdr pair)) (is-list (consp cdr)) (url (if is-list (car cdr) cdr)) )
			(if (not (and (ngender-archive-name-p name) (ngender-archive-url-p url)))
				nil
				(if is-list (cons name url) pair) ) ) ) )

(defun ngender-add-package-archive-pair (symbol new-pair)
	(let* ( (key (car new-pair)) (alist (symbol-value symbol)) (old-pair (assoc key alist)) )
		(if (equal old-pair new-pair) t
			(when old-pair
				(lwarn "changing url for %s from %s to %s in %s" key (cdr old-pair) (cdr new-pair) symbol)
				(set symbol (assoc-delete-all key alist)) )
			(set symbol (cons new-pair (symbol-value symbol))) ) ) )

(defun ngender-add-package-archive-by-key (key)
	(let ( (pair (assoc key *ngender-known-package-archives*)) )
		(cond
			( (symbolp key) (ngender-package-archive-by-key (symbol-name key)) )
			( (not (stringp key)) (lwarn "expected string archive key: %s" key) )
			( (null pair) (lwarn "expected known archive key: %s" key) )
			( t (ngender-add-package-archive-pair 'package-archives pair) ) ) ) )

(defun ngender-package-archive (&rest args)
	"add archives to package-archives and new ones to *ngender-known-package-archives*"
	(dolist (a args)
		(cond
			( (consp a) (let* ( (pair (ngender-archive-p a)) )
										(if (not pair)
											(lwarn "expected archive: %s" a)
											(ngender-add-package-archive-pair '*ngender-known-package-archives* pair)
											(ngender-add-package-archive-pair 'package-archives pair) ) ) )
			( (or (stringp a) (symbolp a)) (ngender-add-package-archive-by-key a) )
			( t (lwarn "expected archive name or pair %s" a) ) ) ) )


(defun ngender-drop-assoc-by-key (symbol key)
  (set symbol (assoc-delete-all key (symbol-value symbol)))
)

(defun ngender-package-archive-delete (key)
	(ngender-drop-assoc-without 'package-archives key)
)

(defun ngender-package-archive-forget (key)
	(ngender-drop-assoc-without 'package-archives key)
	(ngender-drop-assoc-without '*ngender-known-package-archives* key)
)

;; variables
;; package-archives - where to fetch packages from
;; package-load-list - which installed packages to load - might be (all)
;; package-archive-contents - Cache of the contents of the
;;		Emacs Lisp Package Archive.  This is an alist mapping
;;		package names (symbols) to non-empty lists of
;;		`package-desc' structures.

;; functions
;; package-initialize - load packages from package-load-list
;; package-refresh-contents - download descriptions of all packages on package-archives

;; Various package files will use this to ensure
;; that their required packages are loaded.
;; How could we fail gracefully?? This will not!!
(defun ngender-package (&rest packages)
	"ensure these packages are loaded"
	(dolist (p packages)
		(unless (package-installed-p p)
			(package-install p) ) ) )

;; ** File and Directory Support

(defun require-file-path (name type-test type-name &optional parent)
	(let (( path (expand-file-name name (if parent parent *ngender-emacs-home*)) ))
		(let (( p (file-chase-links path) ))
			(if (funcall type-test p) path
				(ngender-warn p type-name '(init)) ) ) ) )

(defun require-file (path &optional parent) (require-file-path path #'file-regular-p 'file parent) )
(defun require-dir (path &optional parent) (require-file-path path #'file-directory-p 'directory parent) )

;; ** Fancy Emacs Load-Path Support

;; The Emacs path will be kept ordered as follows, first to last:
;; (1) User Subdirectories
;; (2) Group (Project) Directories
;; (3) Vendor (3rd party extension) directories
;; (4) Everything that was there initially, including NGender (established in .emacs)

;; Note: it seems that packages add their load directories
;; at the front of load-path!!  Anytime we want to fix this
;; we can call (ngender-rebuild-load-path)

(defvar *ngender-user-subdirectories* '())
(defvar *ngender-group-subdirectories* '())
(defvar *ngender-vendor-subdirectories* '())

(defconst *ngender-path-lists*
	'(*ngender-user-subdirectories* *ngender-group-subdirectories* *ngender-vendor-subdirectories* load-path)
"load-path will be reconstructed from these sublists, deduped, left-to-right; ensure load-path is on this list!" )

(defun ngender-rebuild-load-path ()
  "rebuild emacs load-path with elements of lists named in
*ngender-path-lists* appearing in front in order"
  (setq load-path (delete-dups
	 (apply #'append (mapcar #'symbol-value *ngender-path-lists*)))) )

(defun ngender-path-subdirectories (symbol paths)
	"add directory paths to the front of the list named by
symbol and rebuild emacs load-path"
	(set symbol (ngender-filter-dirs paths))
	(ngender-rebuild-load-path) )

(defun ngender-path-delete (symbol path)
	"delete paths from the list named by symbol and rebuild emacs load-path"
	(set symbol (delete path (symbol-value symbol)))
	(ngender-rebuild-load-path) )

(defun ngender-user-subdirectory (&rest paths)
	"add paths to user subdirectories and rebuild emacs load-path"
	(ngender-path-subdirectories '*ngender-user-subdirectories* paths)
)

(defun ngender-user-subdir-delete (path)
	"delete path from user subdirectories and rebuild emacs load-path"
	(ngender-path-delete '*ngender-user-subdirectories* path)
)

(defun ngender-group-subdirectory (&rest paths)
	"add paths to group subdirectories and rebuild emacs load-path"
	(ngender-path-subdirectories '*ngender-user-subdirectories* paths)
)

(defun ngender-group-subdir-delete (path)
	"delete path from group subdirectories and rebuild emacs load-path"
	(ngender-path-delete '*ngender-group-subdirectories* path)
)

(defun ngender-vendor-subdirectory (&rest paths)
	"add paths to vendor subdirectories and rebuild emacs load-path"
	(ngender-path-subdirectories '*ngender-user-subdirectories* paths)
)

(defun ngender-vendor-subdir-delete (path)
	"delete path from vendor subdirectories and rebuild emacs load-path"
	(ngender-path-delete '*ngender-vendor-subdirectories* path)
)

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
(defun using-gui-p () (and bound-p 'window-system) window-system)

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
		(setq variable-pitch-mode
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
(defun make (n)
  (interactive)
  (compile "make") )

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


;; ** Provide

(provide 'ngender)
