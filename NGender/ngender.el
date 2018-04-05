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
;; for:
;; oddp
;; what else??

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
  (set symbol (seq-filter predicate (ngender-symbol-value symbol))) )

;; *** Using Lists As Sets

(defun ngender-union-sublists (bags)
  "return list set union of list of bag lists"
	(delete-dups (apply #'append bags)) )

(defun ngender-union (&rest bags)
  "return list set union of list of bag lists"
	(ngender-union-sublists bags) )

(defun ngender-update-union-list (symbol items)
  (set symbol (ngender-union (ngender-symbol-value symbol) items) ) )

(defun ngender-update-union (symbol &rest items)
  (ngender-update-union-list symbol items) )

;; *** Managing Path Set Lists

(defun ngender-filter-dirs (paths) (ngender-validate-list (mapcar 'expand-file-name paths) 'file-directory-p 'directory))

(defun ngender-add-paths (symbol paths do-append)
  "prepend or append candidate directory set with directory set bound to symbol"
  (set symbol (let ( (paths (ngender-symbol-value symbol))
		     (newbies (ngender-filter-dirs paths)) )
		(if do-append
		    (ngender-union paths newbies)
		  (ngender-union newbies paths) ) )) )

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
  (ngender-symbol-value '*ngender-known-package-archives*
			'( ("gnu" . "http://elpa.gnu.org/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")
			   ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
			   ("org" . "https://orgmode.org/elpa/")
			   ) ) "package archives known to exist" )

(defvar package-archives
  (ngender-symbol-value 'package-archives '( ("gnu" . "http://elpa.gnu.org/packages/") ) )
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

(defun list-to-associations (list)
	"turn a list of pairs into an association list, i.e. list of cons-cells; any leftover paired with nil"
	(if (null list)
		nil
		(if (consp list)
			(let ((first (car list)) (cdr (cdr list)))
				(if (not (consp cdr))
					list
					(cons (cons first (car cdr)) (list-to-associations (cdr cdr))) ) ) ) ) )

(defun ngender-package-archive (&rest args)
  (if (cl-oddp (length args))
      (ngender-warn args "even list")
    (let* ( (pairs (list-to-associations args))
	    (pair-count (length pairs))
	    (archives (ngender-validate-list pairs #'ngender-archive-p 'archive))
	    (archive-count (length archives)) )
      (and
       (= pair-count archive-count)
       (ngender-update-union-list 'package-archives archives)
       (package-initialize)
       (package-refresh-contents)
       ) ) ) )

(defun ngender-package-archive-delete (key)
  (setq package-archives (assoc-delete-all key package-archives))
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
;; (4) Directories for Packages downloaded from Emacs Repositories

(defvar *ngender-emacs-home* (ngender-symbol-value '*ngender-emacs-home* "~/.emacs.d"))
(defvar *ngender-user-subdirectories* (ngender-symbol-value '*ngender-user-subdirectories*))
(defvar *ngender-group-subdirectories* (ngender-symbol-value '*ngender-group-subdirectories*))
(defvar *ngender-vendor-subdirectories* (ngender-symbol-value '*ngender-vendor-subdirectories*))
(defvar *ngender-subdirectories* (list (require-dir "NGender" *ngender-emacs-home*)))

(defconst *ngender-path-lists*
	'(*ngender-user-subdirectories* *ngender-group-subdirectories* *ngender-vendor-subdirectories* *ngender-subdirectories* load-path)
"load-path will be reconstructed from these sublists, deduped, left-to-right; ensure load-path is on this list!" )

(defun ngender-emacs-home (path)
	(setq *ngender-emacs-home* (require-dir path "~")) )

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

;; *** Customize some directory path lists

;; Can we make any of this generic?
;; How much of this can we discover?
;; Otherwise we should move it out of the NGender Profile!
;; These paths need to be checked for existence!!
;; When should we warn of non-existence??

;; (ngender-prepend-paths (if (boundp 'Info-directory-list)
;; 		     'Info-directory-list 'Info-default-directory-list)
(ngender-prepend-paths 'Info-directory-list
              "/usr/share/info/"
              "/usr/share/info/emacs-24/"
              "/usr/local/share/info/"
              "/usr/share/texmf/doc/info/"
;;              "/usr/local/mercury-0.11.1-beta-2004-06-30/info"
;;              "/opt/gnome/share/info"
              )

(ngender-prepend-paths 'load-path
              "~/Lib/Emacs/"
;;              "~/Lib/Emacs/Scala/"
              "/usr/share/emacs/site-lisp"
;;              "/data/greg/Lib/Emacs/lisp"
;;              "/usr/local/Mercury/lib/mercury/elisp"
;;              "/usr/local/src/flora2/emacs"
;;              "/usr/local/src/XSB/etc"
;;              "/usr/share/doc/ruby-1.6.4/misc"
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
(defun using-gui-p () window-system)

;; ideally minor would be optional, and maybe a sub-minor
;; could be allowed as well?
(defun version>= (major minor)
  (or (> emacs-major-version major)
	  (and (= emacs-major-version major)
		   (>= emacs-minor-version minor) ) ) )

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

;; ** Theme Preferences

;; Themes
(ngender-prepend-paths 'custom-theme-load-path "~/.emacs.d/themes")
(ngender-prepend-paths 'load-path "~/.emacs.d/themes")
;(load-theme 'tomorrow-night-bright t); example - not one I like!

;; ** Preferences for Major Modes

;;; Tcl Preferences

(require 'tcl)

(defun ngender-tcl-preferences ()
  (setq tcl-application "tclsh"
	tcl-indent-level ngender-default-indent
	tcl-continued-indent-level ngender-default-indent
	tcl-use-smart-word-finder t)  )

(add-hook 'tcl-mode-hook 'ngender-pitch-mode)

;;; Require some things in ~/Lib/Emacs

;; ** Printing Support from lynn.el

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

;; ** Markdown Mode

(ngender-update-union 'auto-mode-alist '("\\.mmd\\'" . markdown-mode) )

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
