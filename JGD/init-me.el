;; * JGD/init-me.el
;; Features requiring significant customization
;; are in separate byte-compiled files under
;; - NGender/ if likely to be useful to others
;; - JGD/ otherwise

;; Everything after one or more semicolons is a comment for
;; humans and is ignored by Emacs.

;; This file should be loaded by ~/.emacs
;; which should be linked to ~/.emacs.d/.emacs
;; See README.org file in this directory

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it.
;; Change any of these paths if needed.

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
(ngender-package-archive
	"gnu" "http://elpa.gnu.org/packages/"
	"marmalade" "http://marmalade-repo.org/packages/"
	"melpa-stable" "http://melpa-stable.milkbox.net/packages/"
	"org" "https://orgmode.org/elpa/")

;; Is this redundant given list-packages??
;; Consider moving into appropriate customization file
(defvar my-packages '(auto-complete company helm
	ido-ubiquitous markdown-mode multi-term
	 paredit-everywhere
	persp-mode project-mode rainbow-delimiters racer
	smex toc-org use-package)
  "A list of packages to ensure are installed at launch.")

;; Install the packages we always want:
(apply #'ngender-package my-packages)
;; Failed on ngender.org, Friday 21 April 2017!!

;; ** Font, Face, Pitch, Indentation

(defvar *ngender-pitch-mode* :variable)				; vs. :fixed
(load "ngender-font")

;; ** Load-Paths

;; add-to-list will put the new items at the front so the
;; last added will come first

;; *** Packages under ~/.emacs.d/vendor
;; These are packages or versions of packages not available through the Emacs
;; package system.  Remove them from here and from the vendor directory
;; when no longer needed.
(dolist (path '(	"~/.emacs.d/vendor"
;;								"~/.emacs.d/vendor/ensime/elisp" ;; standard package now good??
									"~/.emacs.d/vendor/html5-el" ) )
	(let ( (p (require-directory path)) )
		(when p (add-to-list 'load-path p) ) ) )

;; *** Personal and NGender Emacs Code Directories
(dolist (path '(	"~/Lib/Emacs" "~/.emacs.d/NGender" "~/.emacs.d/JGD" ) )
	(let ( (p (require-directory path)) )
		(when p (add-to-list 'load-path p) ) ) )

;; ** Load NGender Packages
;; Can/should any of these autoload??

;; Report an error if not found, say nothing if all goes well
(load "ngender" nil t)
(defvar *ngender-org-packages '(mysql-to-org org org-autolist org-bullets org-page
	org-projectile org-tree-slide ox-gfm) "desired set of org-mode packages")
(load "ngender-org" nil t)
(load "ngender-shell" nil t)
(load "ngender-elisp" nil t)
(defvar *ngender-sql-packages* '( sql sql-indent sqlup-mode ))
(load "ngender-sql" nil t)
(load "my-sql-pw" nil t)			 ; encryption would be better!!
(load "ngender-sql-connect" nil t)

;; ** Load JGD Packages

;; Report an error if not found, say nothing if all goes well
;; (load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

;; *** Key Bindings - (Re)Define some keyboard shortcuts

;; ** Load Miscellaneous Simple Packages

;; *** bs - menu for selecting and displaying buffers
(global-set-key "\C-x\C-b" 'bs-show)

;; Magit - Emacs Git integration

(defvar *ngender-magit-packages* '(magit magithub git-commit))
(autoload 'magit-status "ngender-magit")
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
;; (autoload 'ngender-clojure "ngender-clojure")
;; (add-to-list 'auto-mode-alist '("\\.\\(clj[cxs]\\?\\|dtm\\|edn\\)\\'" . ngender-clojure))

;; *** ngender-haskell
;; (autoload 'ngender-haskell "ngender-haskell")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-haskell))

;; **** ngender-mozart
;; (require "ngender-mozart")
;; (autoload 'ngender-mozart "ngender-mozart")
;; (add-to-list 'auto-mode-alist '("\\.oz\\'" . ngender-mozart))

;; **** ngender-php
;; (autoload 'ngender-php "ngender-php")
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . ngender-php))
;; **** ngender-prolog
;; (autoload 'ngender-prolog "ngender-prolog")
;; (add-to-list 'auto-mode-alist '("\\.pl\\'" . ngender-prolog))
;; **** ngender-rust
(defvar *ngender-rust-packages '(rust-mode racer)	"desired set of org-mode packages")
(autoload 'ngender-rust "ngender-rust")
(add-to-list 'auto-mode-alist '("\\.rs\\'" . ngender-rust))
;; **** ngender-shell
;; (autoload 'ngender-shell "ngender-shell")
;; (add-to-list 'auto-mode-alist '("\\.foo\\'" . ngender-shell))

;; ** Ensuring utility buffers exist

;; Not really necessary as they'll be created on demand!
(dolist (buffer '("*SQL*" "*compilation*" "*shell*"))
	(get-buffer-create buffer) )

;; ** Activate simple Global Modes

(winner-mode 1)
(menu-bar-mode 1)              ;  it got disabled somewhere!

;; ** Start the Emacs-Server

(server-start)
