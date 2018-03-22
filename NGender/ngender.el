;; * NGender Emacs Profile Base -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies - provide and require

(provide 'ngender)
(require 'jit-lock)

;; (require 'x-fonts)

;; ** Indentation Issues

;; Q: Why do we prefer indentation by tabs in most modes?
;; A: So we can flexibly change the tab-width!
;; Note: All indentation choices are frought!

;; Define desired indentation in space-equivalents
(defconst ngender-min-indent 1)
(defconst ngender-default-indent 2)
(defconst ngender-default-tab-width 2)

(defun ngender-set-tab-width (n)
  "Set tab-width as a local variable."
  (make-local-variable 'tab-width)
  (setq tab-width (if (< n ngender-min-indent) ngender-default-tab-width n) )
)

(defun ngender-set-default-tab-width ()
  "Set tab-width to our default"
  (ngender-set-tab-width ngender-default-tab-width)
)

(defun ngender-tab-width (n)
  "Set tab-width interactively."
  (interactive "p")
  (ngender-set-tab-width n)
)

;; ** Define some functions for managing lists, sets, paths

;; *** functions for filtering lists

(defun ngender-filter-reverse (items predicate)
  "reversed sublist of items which satisfy predicate"
  (let (results)
    (dolist (item items results)
      (or (apply predicate item) (push item results)) ) ) )

(defun ngender-update-filter-reverse (symbol predicate)
  (set symbol (ngender-filter-reverse (symbol-value symbol) predicate)) )

(defun ngender-filter (predicate items)
  "sublist of items which satisfy predicate"
  (let* ( (results (list 'results)) (tail results) )
    (dolist (item items (cdr results))
      (when (apply predicate item)
	(setcdr tail (list item))
	(setq tail (cdr tail)) ) ) ) )

(defun ngender-update-filter (symbol predicate)
  (set symbol (ngender-filter (symbol-value symbol) predicate)) )

;; *** Using Lists As Sets

(defun ngender-union-list (bag items)
  "union of bag and items"
  (dolist (item items bag)
      (add-to-list 'bag item ) ) )

(defun ngender-update-union (symbol &rest items)
  (set symbol (ngender-union-list (symbol-value symbol) items)) )

;; *** functions for managing paths

;; Would be better to have a filter
;; which would remove and warn
;; of any element already on a path
;; or non-existent in the filesystem.
;; Then just append in front or in back.

(defun ngender-add-paths (symbol paths &optional append)
  "add paths onto existing unique list bound to symbol"
  (dolist (p paths)
      (or (not (file-directory-p p))
				(add-to-list symbol (expand-file-name p) append) ) ) )

(defun ngender-prepend-paths (symbol &rest dirs)
	(or (boundp symbol) (set symbol '()))
	;; assert symbol bound to proper list!!
	(ngender-add-paths symbol dirs) )

(defun ngender-append-paths (symbol &rest dirs)
	(or (boundp symbol) (set symbol '()))
	;; assert symbol bound to proper list!!
	(ngender-add-paths symbol dirs t) )

(defun ngender-str-member (str item &optional delim)
  "is item a member of delimited string?"
  (let ( (d (or delim ":")) )
    (string-match (concat ".*" delim (regexp-quote item) delim ".*")
		  (concat delim str delim) ) ) )

(defun ngender-str-append (str item &optional delim)
  "append item onto delimited string"
  (concat str (or delim ":") item) )

(defun ngender-update-str-append (symbol item &optional delim)
  (set symbol (ngender-str-append (symbol-value 'symbol) item delim)) )

(defun ngender-exec-path-from-shell ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)) ) )

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

(ngender-append-paths 'load-path
	"/usr/share/emacs/site-lisp/"
	"/usr/share/emacs/site-lisp/w3m/"
	)

(ngender-prepend-paths 'load-path
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

