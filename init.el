;;; Emacs Init Code
;; Maintained by jgd
;; NOT byte-compiled
;; Consider migrating content here to jgd.el{,c}

;; Attempted workaround for font prem in 24.3.1
 ;; (setq initial-frame-alist '(
 ;; 	 (font . "Monospace-10")
 ;; 	 (vertical-scroll-bars . right)
 ;; ))
 ;; (setq default-frame-alist '(
 ;;   (font . "Monospace-10")
 ;; 	 (vertical-scroll-bars . right)
 ;; ))

;;; Packages

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

;; Add in your own as you wish:
(defvar my-packages '(
	auto-complete
;	cedet
	cider
; clojure-mode ; comes with cider
; clojure-test-mode ; comes with cider
;	cljdoc
;	cljsbuild-mode
;	clojure-mode-extra-font-locking
;	closure-lint-mode
	company
; company-racer ; something here's been misbehaving!
;	ecb-snapshot
;	enh-ruby-mode
;	emacsql
;	emacsql-mysql
;	emacsql-psql
;	emacsql-sqlite
;	find-file-in-project ; searches project tree
;	flycheck
;	flycheck-clojure
;	flycheck-haskell
;	flycheck-rust
;	ghci-completion
	git-commit
	helm
;	idle-highlight-mode ; does what?
	ido-ubiquitous
	magit
	magithub
	markdown-mode
	multi-term
	mysql-to-org
	org
	org-autolist
	org-bullets
;	org-context
	org-page
	org-projectile
;	org-ref
	org-tree-slide
	ox-gfm
	paredit-everywhere
;	perl6-mode
;;	perspective
	persp-mode
	php-mode
	project-mode
;	prolog
	rainbow-delimiters
	racer
	rust-mode
;	scala-mode
;	scala-mode2
	sql-indent
	sqlup-mode
	smex
;	tldr
	toc-org
	typed-clojure-mode
	use-package
;	workgroups2
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

(dolist (p '(
	     "~/Lib/Emacs" "~/.emacs.d/vendor"
	     "~/.emacs.d/jgd" "~/.emacs.d/vendor/ensime/elisp/"
	     "~/.emacs.d/vendor/html5-el/" ) )
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

;; WindMove

(windmove-default-keybindings 'meta)

;; Uniquify
(require 'uniquify)


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
; (global-company-mode)

;; Reduce the time after which the company auto completion popup opens
; (setq company-idle-delay 0.2)

;; Reduce the number of characters before company kicks in
; (setq company-minimum-prefix-length 1)

;; Racer

;; Set path to racer binary
(setq racer-cmd "/usr/local/bin/racer")

;; Set path to rust src directory
; (setq racer-rust-src-path "~/.rust/src/")
(setq racer-rust-src-path "/usr/local/lib/rustlib/")

;; Load rust-mode when you open `.rs` files
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Setting up configurations when you load rust-mode
(add-hook 'rust-mode-hook
	  '(lambda ()
	     ;; Enable racer
	     (racer-activate)
	     (racer-mode)
	     
	     ;; Hook in racer with eldoc to provide documentation
	     (racer-turn-on-eldoc)
	     
	     ;; Use flycheck-rust in rust-mode
	     ; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
	     
	     ;; Use company-racer in rust mode
	     ;; (set (make-local-variable 'company-backends) '(company-racer))
	     
	     ;; Key binding to jump to method definition
	     (local-set-key (kbd "M-.") #'racer-find-definition)
	     
	     ;; Key binding to auto complete and indent
	     (local-set-key (kbd "TAB") #'racer-complete-or-indent)
	     ) )


;;; Tramp

(setq tramp-default-method "ssh")

;;; Miscellaneous

; Somehow M-; got rebound to something weird so reset it
(global-set-key (kbd "M-,") 'tags-loop-continue)

;;; workgroups

(setq wg-morph-on nil)

;;; workgroups2

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
