;; * init-examples.el

;; Do not load this file!
;; Copy the snippets you like into your own init file,
;; e.g. User-Me/init-me.el

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it!

;; ** TODO: Add more snippets for nice features!

;; ** Debugging: get more information from emacs
(setq force-load-messages t)
(setq debug-on-error t)

;; ** Switch to a more extensive Package Archive

(ngender-clear-package-archives)
(ngender-package-archive "melpa-stable")

;; Some nice packages not yet available through modules

(ngender-package auto-complete)

(ngender-package company)

(ngender-package ido-ubiquitous)

;; *** Multi-Term - manage multiple terminal emulators
(ngender-package multi-term)
(require 'multi-term)

(ngender-package paredit-everywhere)

(ngender-package rainbow-delimiters)

(ngender-package racer)

(ngender-package smex)

(ngender-package use-package)

;; ** Themes

;; You have to download the themes you want first
;; TODO: More instruction needed here!

(dolist (d '("~/.emacs.d/themes" "~/.emacs.d/User-Me/themes"))
	(when (file-directory-p d)
		(ngender-prepend-paths 'custom-theme-load-path d) ) )

;; Example of loading a theme:
(load-theme 'tomorrow-night-bright t)

;; ** NGender Modules

;; Org-Mode is one of the Emacs "Killer Features"
;; See http://orgmode.org
(ngender org mysql autolist bullets page projectile tree-slide ox-gfm toc)

;; Magit - Emacs Git integration!

;; The magit module still uses an external list
(defvar *ngender-magit-packages* '(magit magithub git-commit))
;; (autoload 'magit-status "ngender-magit") ; does not work!!??
(ngender magit)

;; A simple shell mode
(ngender shell)

;; Support for hacking Emacs-Lisp
(ngender elisp)

;; sql still uses an external feature list:
(defvar *ngender-sql-packages* '( sql sql-indent sqlup-mode ))
(ngender sql-accounts)		; copy to User-Me and customize!!

;; bs - menu for selecting and displaying buffers
(global-set-key "\C-x\C-b" 'bs-show)

;; ** Configuration of some Built-In Packages

(require 'windmove)
(windmove-default-keybindings 'meta)

;; *** Uniquify - better buffer names on conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; *** Tramp
(setq tramp-default-method "ssh")

;; ** Some Nice Bindings

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

;; ** Set some miscellaneous variables

(setq visible-bell t)
(setq tab-stop-list nil)
(setq inhibit-startup-screen t)

;; ** Misc Simple modules

(ngender beacon)

;; ** Activate simple Global Modes

;; allows restoration of your window layout
(winner-mode 1)

;; if you don't want a  menu bar
(menu-bar-mode 0)

;; if you lose your menu bar and want it back
(menu-bar-mode 1)
