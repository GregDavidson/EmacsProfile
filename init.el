;;; Emacs Init Code
;; Maintained by jgd
;; NOT byte-compiled
;; Consider migrating content here to jgd.el{,c}

;; Attempted workaround for font problem in 24.3.1
 (setq initial-frame-alist '(
	 (font . "Monospace-10")
	 (vertical-scroll-bars . right)
 ))
 (setq default-frame-alist '(
   (font . "Monospace-10")
	 (vertical-scroll-bars . right)
 ))

;;; Packages

(require 'package)
(defvar my-archives
	'( ("marmalade" . "http://marmalade-repo.org/packages/")
		 ("tromey" . "http://tromey.com/elpa/")
		 ("melpa" . "http://melpa.milkbox.net/packages/") ) )
(dolist (a my-archives)	(add-to-list 'package-archives a t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(
;;                      clojure-mode ; comes with cider
;;                      clojure-test-mode ; comes with cider
;;	cedet
	cider
	company
	company-racer
	company
	multi-term
	emacsql
	emacsql-mysql
	emacsql-psql
	emacsql-sqlite
	org
	mysql-to-org
	php-mode
	sql-indent
	sqlup-mode
	workgroups2
	rust-mode
;;	scala-mode2
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

;;; Load-Paths

(dolist (p '( "~/Lib/Emacs" "~/.emacs.d/vendor"
							"~/.emacs.d/jgd" "~/.emacs.d/vendor/ensime/elisp/"
							"~/.emacs.d/vendor/html5-el/"))
	(add-to-list 'load-path p) )

;;(load-file "~/.emacs.d/cedet/cedet-devel-load.el")
;;(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")
;;(load "vendor/cedet/cedet-devel-load")
;;(load "vendor/cedet/contrib/cedet-contrib-load")

;;; Ensime

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; Load some more code

;; Report an error if not found, say nothing if all goes well
(load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

; (load "frame-bufs" nil t)
; (frame-bufs-mode t)

;; (require 'felineherd) ;; what is this?

;; (when window-system (speedbar t))

;;; Key Bindings - (Re)Define some keyboard shortcuts

(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-cv" 'buffer-face-mode)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Org Mod

;; http://wenshanren.org/?p=334 Friday 18 November 2016 s/src/source/
(defun org-insert-source-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
		(let ((src-code-types
						'("emacs-lisp" "python" "C" "sh" "java" "js"
							 "clojure" "C++" "css" "calc" "asymptote"
							 "dot" "gnuplot" "ledger" "lilypond" "mscgen"
							 "octave" "oz" "plantuml" "R" "sass" "screen"
							 "sql" "awk" "ditaa" "haskell" "latex" "lisp"
							 "matlab" "ocaml" "org" "perl" "ruby" "scheme"
							 "sqlite" ) ))
			(list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code) ) )

(global-set-key (kbd "C-M-'") 'org-insert-source-block)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun org-mode-jgd ()
	(setq variable-pitch-mode 1)
	(set-face-attribute 'org-table nil :inherit 'fixed-pitch) )

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'org-mode-jgd)

;; Variable Pitch Mode for Source Code

(defun variable-pitch-mode-jgd ()
	(setq variable-pitch-mode 1)
)

;; SQL Mode

(add-hook 'sql-mode-hook 'variable-pitch-mode-jgd)

;; Uniquify
(require 'uniquify)

;;; workgroups2

; package is a bit old - is there something better???
(require 'workgroups2)

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
(setq wg-prefix-key (kbd "C-s-RET"))
;(setq wg-prefix-key (kbd "C-s-<return>"))

;; Change workgroups session file
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
(global-set-key (kbd "C-s-<down>") 'wg-save-session)
(global-set-key (kbd "C-s-<up>") 'wg-load-session)
(global-set-key (kbd "C-M-s-<up>") 'wg-reload-session)
(global-set-key (kbd "C-s-<right>") 'wg-switch-to-workgroup)
(global-set-key (kbd "C-s-<left>") 'wg-switch-to-previous-workgroup)

(workgroups-mode 1)

;; Workaround to hopefully get rid of annoying:
;; Save Error: "Unbalanced parentheses": /home/greg/.emacs.d/semanticdb/!home!greg!Play!Lang!Lisps!ClojureScript!modern-cljs!resources!public!semantic.cache
; This was working, but now:
; Symbol's function definition is void: global-semantic-show-unmatched-syntax-mode
; (global-semantic-show-unmatched-syntax-mode -1)

;; html5 editing

(eval-after-load "rng-loc"
  '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

; This is not working on ngender.org, Friday 21 April 2017, !!!
; (require 'whattf-dt)

;; this font works via set-frame-font so let's write a function
;; using format to put in the desired size!
;; -unknown-DejaVu Sans-normal-normal-semicondensed-*-*-*-*-*-*-0-iso10646-1


;;;; j-lang Support
;; https://github.com/zellio/j-mode

(add-to-list 'load-path "~/.emacs.d/j-mode/")
(autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
(add-to-list 'auto-mode-alist '("\\.ij[rstp]$" . j-mode))


;;;; Rust Support
; http://bassam.co/emacs/2015/08/24/rust-with-emacs/

;; Company

;; Enable company globally for all mode
(global-company-mode)

;; Reduce the time after which the company auto completion popup opens
(setq company-idle-delay 0.2)

;; Reduce the number of characters before company kicks in
(setq company-minimum-prefix-length 1)

;; Racer

;; Set path to racer binary
(setq racer-cmd "/usr/local/bin/racer")

;; Set path to rust src directory
(setq racer-rust-src-path "~/.rust/src/")

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook
	
	'(lambda ()
     ;; Enable racer
;;     (racer-activate)
		 (racer-mode)
		 
		 ;; Hook in racer with eldoc to provide documentation
     (racer-turn-on-eldoc)
		 
		 ;; Use flycheck-rust in rust-mode
     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
		 
		 ;; Use company-racer in rust mode
     (set (make-local-variable 'company-backends) '(company-racer))
		 
		 ;; Key binding to jump to method definition
     (local-set-key (kbd "M-.") #'racer-find-definition)
		 
		 ;; Key binding to auto complete and indent
     (local-set-key (kbd "TAB") #'racer-complete-or-indent) ) )

;; Tramp

(setq tramp-default-method "ssh")

;; Shell Mode

;; Work in progress!!!
;; Need to learn about the faces in use in shell mode!

(defun shell-mode-jgd ()
	(setq variable-pitch-mode 1)
//	(set-face-attribute 'ansi-color-face nil :inherit 'fixed-pitch)
 )

(add-hook 'shell-mode-hook 'turn-on-font-lock)
(add-hook 'shell-mode-hook 'shell-mode-jgd)

;; Miscellaneous

; Somehow M-; got rebound to something weird so reset it
(global-set-key (kbd "M-,") 'tags-loop-continue)
