;; * User-Me/init-me.el
;; Features requiring significant customization
;; are in separate byte-compiled files under
;; - NGender/ if likely to be useful to others
;; - User-Me/ otherwise

;; Everything after one or more semicolons is a comment for
;; humans and is ignored by Emacs.

;; This file should be loaded by ~/.emacs
;; which should be linked to ~/.emacs.d/.emacs
;; See README.org file in this directory

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it.
;; Change any of these paths if needed.

;; ** Packages

(require 'package)
(require 'ngender)
(ngender-package-archive "gnu" "marmalade" "melpa-stable" "org")

;; Consider moving into appropriate customization file
(apply #'ngender-package '(auto-complete company
														ido-ubiquitous multi-term
														paredit-everywhere
														rainbow-delimiters racer
														smex use-package))

;; ** Themes

(dolist (d '("~/.emacs.d/themes" "~/.emacs.d/User-Me/themes"))
	(when (file-directory-p d)
		(ngender-prepend-paths 'custom-theme-load-path d)
		(ngender-prepend-paths 'load-path d) ) )
;(load-theme 'tomorrow-night-bright t); example - not one I like!

;; ** Preferences for Major Modes

;; ** Font, Face, Pitch, Indentation

;; uncomment one of these two options:
;; (defvar *ngender-pitch-mode* :fixed)
(defvar *ngender-pitch-mode* :variable)
(require 'ngender-font)

;; ** LoadNGender Packages
;; Can/should any of these autoload??

(defvar *ngender-org-packages* '(mysql-to-org org org-autolist org-bullets org-page
	org-projectile org-tree-slide ox-gfm toc-org) "desired set of org-mode packages")
(require 'ngender-org)
(require 'ngender-shell)
(require 'ngender-elisp)
(defvar *ngender-sql-packages* '( sql sql-indent sqlup-mode ))
(require 'ngender-sql)
(require 'my-sql-pw)			 ; encryption would be better!!
(require 'ngender-sql-connect)

;; ** Load JGD Packages

;; Report an error if not found, say nothing if all goes well
;; (load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

;; *** Key Bindings - (Re)Define some keyboard shortcuts

;; ** Load Miscellaneous Simple Packages

;; ** Markdown Mode

;; This looks like boilerplate - how should we automate it??
(defun ngender-markdown-mode ()
	(ngender-package markdown-mode)
	(markdown-mode) )
(ngender-update-union-with-items 'auto-mode-alist '("\\.mmd\\'" . ngender-markdown-mode) )

;; *** bs - menu for selecting and displaying buffers
(global-set-key "\C-x\C-b" 'bs-show)

;; Magit - Emacs Git integration

(defvar *ngender-magit-packages* '(magit magithub git-commit))
(autoload 'magit-status "ngender-magit")

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
(require 'ngender-tcl)
(require 'ngender-printing)

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
(defvar *ngender-rust-packages* '(rust-mode racer)	"desired set of org-mode packages")
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
