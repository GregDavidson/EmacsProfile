;; * Old or Speculative Customizations which have been
;; - disabled
;; - commented out
;; - or otherwise are not in use!


;; (require 'frame-bufs)
;; (load-file "~/Lib/Emacs/frame-bufs.elc")
;; (frame-bufs-mode t)

; (load "frame-bufs" nil t)
; (frame-bufs-mode t)

;; (require 'felineherd) ;; what is this?

;; (when window-system (speedbar t))

;;(load-file "~/.emacs.d/cedet/cedet-devel-load.el")
;;(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")
;;(load "vendor/cedet/cedet-devel-load")
;;(load "vendor/cedet/contrib/cedet-contrib-load")

;; Is this redundant given list-packages??
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
