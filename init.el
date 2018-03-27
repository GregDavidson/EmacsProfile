;; * Emacs Init Code
;; Maintained by jgd
;; NOT byte-compiled
;; Features requiring significant customization
;; are in separate byte-compiled files under
;; - NGender/ if likely to be useful to others
;; - JGD/ otherwise

;; ** Attempted workaround for font prem in 24.3.1
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
;; Consider moving into appropriate customization file
(defvar my-packages '(auto-complete company git-commit helm
	ido-ubiquitous magit magithub markdown-mode multi-term
	mysql-to-org org org-autolist org-bullets org-page
	org-projectile org-tree-slide ox-gfm paredit-everywhere
	persp-mode php-mode project-mode rainbow-delimiters racer
	rust-mode sql sql-indent sqlup-mode smex toc-org use-package)
  "A list of packages to ensure are installed at launch.")

;; How to complain if package can't be loaded??
(defun ngender-package-loaded (&rest packages)
	"ensure that these packages are loaded"
	(dolist (p packages)
		(unless (package-installed-p p)
			(package-install p) ) )
)

;; This is not working on ngender.org, Friday 21 April 2017, !!!
(apply 'ngender-package-loaded my-packages)
;; various JGD and NGender package files will use this
;; to ensure that their required packages are loaded

;; ** Font and Pitch Preferences

(defvar *ngender-pitch-mode* :variable)
(defvar *ngender-fixed-font* "Dejavu Sans Mono")
(defvar *ngender-variable-font* "Dejavu Sans Condensed")
(defvar *ngender-font* *ngender-variable-font*)
;; (defvar *ngender-font* *ngender-fixed-font*)
(defvar *ngender-frame-font* *ngender-font*)
;		 "-adobe-courier-medium-r-normal--%d-*-*-*-m-*-iso8859-1"

;; Switches face to value of buffer-face-mode-face
;; If default is fixed-pitch, set to variable-pitch and vice versa!
(global-set-key "\C-cv" 'buffer-face-mode)
(setq buffer-face-mode-face 'fixed-pitch)
;; (setq buffer-face-mode-face 'variable-pitch)

;; ** Load-Paths

;; *** Packages under ~/.emacs.d/vendor
;; These are packages or versions of packages not available through the Emacs
;; package system.  Remove them from here and from the vendor directory
;; when no longer needed.
(dolist (p '(	"~/.emacs.d/vendor"
;;						"~/.emacs.d/vendor/ensime/elisp/" ;; standard package now good?
							"~/.emacs.d/vendor/html5-el/" ) )
  (add-to-list 'load-path p) )

;; *** Personal and NGender Emacs Code Directories
(dolist (p '(	"~/Lib/Emacs" "~/.emacs.d/NGender" "~/.emacs.d/JGD" ) )
  (add-to-list 'load-path p) )

;; ** Load NGender Packages
;; Can/should any of these autoload??

;; Report an error if not found, say nothing if all goes well
(load "ngender" nil t)
(load "ngender-rx" nil t)
;; (load "ngender-org" nil t)
(load "ngender-shell" nil t)
(load "ngender-elisp" nil t)
(load "ngender-sql" nil t)
(load "my-sql-pw" nil t)
(load "ngender-sql-connect" nil t)

;; ** Load JGD Packages

;; Report an error if not found, say nothing if all goes well
;; (load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

;; *** Key Bindings - (Re)Define some keyboard shortcuts

;; ** Load Miscellaneous Simple Packages

;; *** bs - menu for selecting and displaying buffers
(global-set-key "\C-x\C-b" 'bs-show)

;; Magit - Emacs Git integration

;; Package will autoload
(global-set-key (kbd "C-x g") 'magit-status)

;; *** Multi-Term - manage multiple terminal emulators

(require 'multi-term)

;; *** WindMove - move among windows with arrow keys

(require 'windmove)
(windmove-default-keybindings 'meta)

;; *** Uniquify - better buffer names on conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Workaround to hopefully get rid of annoying:
;; Save Error: "Unbalanced parentheses": /home/greg/.emacs.d/semanticdb/!home!greg!Play!Lang!Lisps!ClojureScript!modern-cljs!resources!public!semantic.cache
; This was working, but now:
; Symbol's function definition is void: global-semantic-show-unmatched-syntax-mode
; (global-semantic-show-unmatched-syntax-mode -1)

;; *** html5 editing - needs review!!

;; How is this related to "~/.emacs.d/vendor/html5-el/" ??

;; (eval-after-load "rng-loc"
;;   '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

; This is not working on ngender.org, Friday 21 April 2017, !!!
; (require 'whattf-dt)

;; this font works via set-frame-font so let's write a function
;; using format to put in the desired size!
;; -unknown-DejaVu Sans-normal-normal-semicondensed-*-*-*-*-*-*-0-iso10646-1

;; *** J-Lang Support - needs review!!
;; https://github.com/zellio/j-mode

;; Check that these paths are up to date next time we play with J!

;; (add-to-list 'load-path "~/.emacs.d/j-mode/")
;; (autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
;; (add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))

;; *** Company - completion system - needs review!!

;; Do we still want this??

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

; (workgroups-mode 1)

;; ** Key Settings

;; *** ignore keysyms generated by my flakey Dell Studio
(global-set-key [XF86AudioMedia] 'ignore)
(global-set-key [C-XF86AudioMedia] 'ignore)
(global-set-key [M-XF86AudioMedia] 'ignore)
(global-set-key [XF86AudioPrev] 'ignore)
(global-set-key [C-XF86AudioPrev] 'ignore)

;; Make Control-W kill the word to the left of the cursor
;; just like it works within a shell command line.
(global-set-key [?\C-w] 'backward-kill-word)
(global-set-key [C-backspace] 'kill-region) ; was C-W

;; the "*-region-or-word" functions don't seem to exist in gnu emacs
;; (global-set-key [?\M-c] 'capitalize-region-or-word)
;; (global-set-key [?\M-C] 'upcase-region-or-word)

;; *** Super bindings
(global-set-key [?\s-c] 'upcase-word)
(global-set-key [?\s-\;] 'comment-region)
(global-set-key [?\s-\:] 'uncomment-region)

;; *** Bindings for Control plus a function key:

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

;; (require 'jgd)
;; (require 'ngender)
;; (require 'ngender-org)
;; (add-to-list 'auto-mode-alist '("\\.org\\'" . ngender-org-mode)
;; (require 'ngender-sql)
;; (add-to-list 'auto-mode-alist '("\\.sql\\'" . ngender-sql-mode)
;; (require 'ngender-sql-connect)
;; (require 'my-sql-pw)

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
;; (autoload 'ngender-c-mode "ngender-c")
;; (add-to-list 'auto-mode-alist '("\\.[cChH]\\(pp\\|PP\\|xx\\|XX\\|++\\|\\)\\'" . ngender-c-mode)

;; *** ngender-clojure
;; (autoload 'ngender-clojure-mode "ngender-clojure")
;; (add-to-list 'auto-mode-alist '("\\.\\(clj[cxs]\\?\\|dtm\\|edn\\)\\'" . ngender-clojure-mode))

;; *** ngender-haskell
;; (autoload 'ngender-haskell-mode "ngender-haskell")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-haskell-mode))

;; **** ngender-mozart
;; (require "ngender-mozart")
;; (autoload 'ngender-mozart-mode "ngender-mozart")
;; (add-to-list 'auto-mode-alist '("\\.oz\\'" . ngender-mozart-mode)

;; **** ngender-php
;; (autoload 'ngender-php-mode "ngender-php")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-php-mode)
;; **** ngender-prolog
;; (autoload 'ngender-prolog-mode "ngender-prolog")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-prolog-mode)
;; **** ngender-rust
;; (autoload 'ngender-rust-mode "ngender-rust")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-rust-mode)
;; **** ngender-rx
;; (autoload 'ngender-rx-mode "ngender-rx")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-rx-mode)
;; **** ngender-shell
;; (autoload 'ngender-shell-mode "ngender-shell")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-shell-mode)

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; ** Ensuring utility buffers exist

;; Not really necessary as they'll be created on demand!
(dolist (buffer '("*SQL*" "*compilation*" "*shell*"))
	(get-buffer-create buffer) )

;; ** Activate simple Global Modes

(winner-mode 1)
(menu-bar-mode 1)              ;  it got disabled somewhere!

;; ** Start the Emacs-Server

(server-start)
