;; * Emacs Init Code
;; Maintained by jgd
;; NOT byte-compiled
;; Anything complicated should be migrated to a byte-compiled file
;; under NGender if it's fairly generic
;; under JGD otherwise

;; Attempted workaround for font prem in 24.3.1
 ;; (setq initial-frame-alist '(
 ;; 	 (font . "Monospace-10")
 ;; 	 (vertical-scroll-bars . right)
 ;; ))
 ;; (setq default-frame-alist '(
 ;;   (font . "Monospace-10")
 ;; 	 (vertical-scroll-bars . right)
 ;; ))

;; ** Packages

(require 'package)
(defvar my-archives
	'( ("gnu" . "http://elpa.gnu.org/packages/")
		 ("marmalade" . "http://marmalade-repo.org/packages/")
		 ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
		 ("org" . "https://orgmode.org/elpa/") ) )
(dolist (a my-archives)	(add-to-list 'package-archives a t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents) )

;; Is this redundant given list-packages??
(defvar my-packages '(
	auto-complete
	cider
	company
	git-commit
	helm
	ido-ubiquitous
	magit
	magithub
	markdown-mode
	multi-term
	mysql-to-org
	org
	org-autolist
	org-bullets
	org-page
	org-projectile
	org-tree-slide
	ox-gfm
	paredit-everywhere
	persp-mode
	php-mode
	project-mode
	rainbow-delimiters
	racer
	rust-mode
	sql-indent
	sqlup-mode
	smex
	toc-org
	typed-clojure-mode
	use-package
)
  "A list of packages to ensure are installed at launch.")

; This is not working on ngender.org, Friday 21 April 2017, !!!
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p) ) )

(require 'cider)
(require 'multi-term)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(setq cider-repl-popup-stacktraces t)

;; (setq cider-repl-history-file "path/to/file")

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; ** Font and Pitch Preferences

(defvar *ngender-pitch-mode* :variable)
(defvar *ngender-fixed-font* "Dejavu Sans Mono")
(defvar *ngender-variable-font* "Dejavu Sans Condensed")
(defvar *ngender-font* *ngender-variable-font*)
(defvar *ngender-frame-font* *ngender-font*)
;		 "-adobe-courier-medium-r-normal--%d-*-*-*-m-*-iso8859-1"

;; ** Load-Paths

(dolist (p '(
							"~/Lib/Emacs" "~/.emacs.d/vendor"
							"~/.emacs.d/JGD" "~/.emacs.d/NGender"
							"~/.emacs.d/vendor/ensime/elisp/"
							"~/.emacs.d/vendor/html5-el/" ) )
	(add-to-list 'load-path p) )

;; ** Load Packages

;; *** Load NGender Packages
;; Should instead arrange for most of them to autoload!!

;; Report an error if not found, say nothing if all goes well
(load "ngender" nil t)
(load "ngender-cut1" nil t)
(load "ngender-rx" nil t)
(load "ngender-org" nil t)
(load "ngender-shell" nil t)
(load "ngender-elisp" nil t)
(load "ngender-sql" nil t)
(load "ngender-sql-pw" nil t)
(load "ngender-sql-connect" nil t)

;; *** Load JGD Packages

;; Report an error if not found, say nothing if all goes well
(load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

;; *** Key Bindings - (Re)Define some keyboard shortcuts

(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-cv" 'buffer-face-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;; *** WindMove

(windmove-default-keybindings 'meta)

;; *** Uniquify
(require 'uniquify)


;; Workaround to hopefully get rid of annoying:
;; Save Error: "Unbalanced parentheses": /home/greg/.emacs.d/semanticdb/!home!greg!Play!Lang!Lisps!ClojureScript!modern-cljs!resources!public!semantic.cache
; This was working, but now:
; Symbol's function definition is void: global-semantic-show-unmatched-syntax-mode
; (global-semantic-show-unmatched-syntax-mode -1)

;; *** html5 editing

(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

; This is not working on ngender.org, Friday 21 April 2017, !!!
; (require 'whattf-dt)

;; this font works via set-frame-font so let's write a function
;; using format to put in the desired size!
;; -unknown-DejaVu Sans-normal-normal-semicondensed-*-*-*-*-*-*-0-iso10646-1


;; *** j-lang Support
;; https://github.com/zellio/j-mode

(add-to-list 'load-path "~/.emacs.d/j-mode/")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

;; *** Company

;; Enable company globally for all mode
; (global-company-mode)

;; Reduce the time after which the company auto completion popup opens
; (setq company-idle-delay 0.2)

;; Reduce the number of characters before company kicks in
; (setq company-minimum-prefix-length 1)

;; *** Tramp

(setq tramp-default-method "ssh")

;; ** Miscellaneous

; Somehow M-; got rebound to something weird so reset it
(global-set-key (kbd "M-,") 'tags-loop-continue)

;; *** workgroups

(setq wg-morph-on nil)

;; *** workgroups2

;; Deleted this package Aug 2 2017 because
;; trying to get persp-mode to work
;; and this this is not compatible
;; Comments with ";; ; " are how it was last

;; package is a bit old - is there something better???
;; ; (require 'workgroups2)

;; <prefix> <key>
;; 
;; <prefix> c    - create workgroup
;; <prefix> A    - rename workgroup
;; <prefix> k    - kill workgroup
;; <prefix> v    - switch to workgroup
;; <prefix> C-s  - save session
;; <prefix> C-f  - load session

;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

;; Change prefix key (before activating WG)
;; ; (setq wg-prefix-key (kbd "C-s-RET"))
;; (setq wg-prefix-key (kbd "C-s-<return>"))

;; Change workgroups session file
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
;; ;  (global-set-key (kbd "C-s-<down>") 'wg-save-session)
;; ;  (global-set-key (kbd "C-s-<up>") 'wg-load-session)
;; ;  (global-set-key (kbd "C-M-s-<up>") 'wg-reload-session)
;; ;  (global-set-key (kbd "C-s-<right>") 'wg-switch-to-workgroup)
;; ;  (global-set-key (kbd "C-s-<left>") 'wg-switch-to-previous-workgroup)

; (workgroups-mode 1)

;; ** Capture-Mode

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
                           
;; ** Key Settings

;; ignore keysyms generated by my flakey Dell Studio
(global-set-key [XF86AudioMedia] 'ignore)
(global-set-key [C-XF86AudioMedia] 'ignore)
(global-set-key [M-XF86AudioMedia] 'ignore)
(global-set-key [XF86AudioPrev] 'ignore)
(global-set-key [C-XF86AudioPrev] 'ignore)


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

(global-set-key [?\s-c] 'upcase-word)
(global-set-key [s-prior] 'ngender-previous-window)
(global-set-key [s-next] 'ngender-next-window)
(global-set-key [?\s-\;] 'comment-region)
(global-set-key [?\s-\:] 'uncomment-region)

;; *** Bindings for Meta (aka Alt) plus a function key:

(global-set-key [C-f1] 'help-command)	; press f1 for help
(global-set-key [C-f2] 'shell)	; get a Unix shell in a window
(global-set-key [C-f3] 'compile)	; compile (make) something
(global-set-key [C-f4] 'next-error)	; visit the next compilation error
(global-set-key [C-f5] 'undo)		; Keep pressing it to undo more
(global-set-key [C-f7] 'blink-matching-open)	;  matching parens
(global-set-key [C-f8] 'start-kbd-macro)	; start recording commands
(global-set-key [C-f9] 'end-kbd-macro)	; finish and call it a macro
(global-set-key [C-f10] 'call-last-kbd-macro)	; re-execute the macro
;; (global-set-key [M-f11] 'upcase-word)
(global-set-key [C-f12] 'repeat-complex-command)

;; *** Set some miscellaneous variables
(setq visible-bell t)
(setq tab-stop-list nil)
(setq inhibit-startup-screen t)

;; ** Loading desired NGender and JGD Modules

(require 'jgd)
(require 'ngender)
(require 'ngender-org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . ngender-org-mode)
(require 'ngender-sql)
(add-to-list 'auto-mode-alist '("\\.sql\\'" . ngender-sql-mode)
(require 'ngender-sql-connect)
(require 'my-sql-pw)

;; ** Not Yet Loading NGender and JGD Modules
;; *** ngender-cedet
;; (require 'ngender-cedet)
;; *** ngender-clisp
;; (require 'ngender-clisp)
;; *** ngender-sql-test
;; (require 'ngender-sql-test)
;; *** ox-tiki
;; (require 'ox-tiki)

;; ** AutoLoading NGender Modules

;; *** ngender-c
(autoload 'ngender-c-mode "ngender-c")
(add-to-list 'auto-mode-alist '("\\.[cChH]\\(pp\\|PP\\|xx\\|XX\\|++\\|\\)\\'" . ngender-c-mode)

;; *** ngender-clojure
(autoload 'ngender-clojure-mode "ngender-clojure")
(add-to-list 'auto-mode-alist '("\\.\\(clj[cxs]\\?\\|dtm\\|edn\\)\\'" . ngender-clojure-mode))

;; *** ngender-haskell
(autoload 'ngender-haskell-mode "ngender-haskell")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-haskell-mode))

;; **** ngender-mozart
;; (require "ngender-mozart")
;; (autoload 'ngender-mozart-mode "ngender-mozart")
;; (add-to-list 'auto-mode-alist '("\\.oz\\'" . ngender-mozart-mode)

;; **** ngender-php
(autoload 'ngender-php-mode "ngender-php")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-php-mode)
;; **** ngender-prolog
(autoload 'ngender-prolog-mode "ngender-prolog")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-prolog-mode)
;; **** ngender-rust
(autoload 'ngender-rust-mode "ngender-rust")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-rust-mode)
;; **** ngender-rx
(autoload 'ngender-rx-mode "ngender-rx")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-rx-mode)
;; **** ngender-shell
(autoload 'ngender-shell-mode "ngender-shell")
(add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-shell-mode)

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ** Ensuring utility buffers exist

; Let's make sure that these handy buffers exist
(dolist (buffer '("*SQL*" "*compilation*" "*shell*"))
	(get-buffer-create buffer) )


;; ** Last things

;; *** Server Start

(server-start)

;; *** Winner Mode

(winner-mode 1)

;; *** Menu Bar Mode

(menu-bar-mode 1)              ;  it got disabled somewhere!
