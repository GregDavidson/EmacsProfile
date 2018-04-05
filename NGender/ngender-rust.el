;; * Rust Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(require 'ngender)

(defvar *ngender-rust-packages*
	(if (boundp '*ngender-rust-packages*) *ngender-rust-packages*'(rust-mode))
	"minimal set of rust packages")
(apply #'ngender-package *ngender-rust-packages*)

(unless (boundp '*ngender-rust-racer-p*)
	(defvar *ngender-rust-racer-p* nil "use racer with rust") )
(unless (boundp '*ngender-rust-flycheck-p*)
	(defvar *ngender-rust-flycheck-p*
		(member 'flycheck-mode *ngender-rust-packages*)
			"use flycheck-mode with rust" ) )
(unless (boundp '*ngender-rust-company-p*)
	(defvar *ngender-rust-company-p*
		(member 'flycheck-mode *ngender-rust-packages*)
		"use company-mode with rust") )

;; ** Racer for Rust

(when (member 'racer *ngender-rust-packages*)

	(let ( src (cmd (require-file (executable-find "racer" "racer binary"))) )

		(dolist (d '("~/.rust/src/" "/usr/local/lib/rustlib/"))
			(if (file-directory-p d) (setq src d)) )

		(require-dir src)
		(require-file cmd)
		(when (and src cmd)	(setq *ngender-rust-racer-p* t))

		(when *ngender-rust-racer-p*
			(defvar racer-cmd cmd "racer binary")
			(defvar racer-rust-src-path src "rust src dir") )

;;;; Rust Support reference
; http://bassam.co/emacs/2015/08/24/rust-with-emacs/

(defun ngender-rust ()
	(when *ngender-rust-racer-p*
		;; Enable racer
		(racer-activate)
		(racer-mode)
		;; Key binding to jump to method definition
		(local-set-key (kbd "M-.") #'racer-find-definition)
		;; Key binding to auto complete and indent
		(local-set-key (kbd "TAB") #'racer-complete-or-indent)
		;; Hook in racer with eldoc to provide documentation
		(racer-turn-on-eldoc) )
	(when *ngender-rust-flycheck-p*
		(add-hook 'flycheck-mode-hook #'flycheck-rust-setup) )
	(when *ngender-rust-company-p*
		(set (make-local-variable 'company-backends) '(company-racer)) )
	(ngender-pitch-mode)
	(setq orgstruct-heading-prefix-regexp "/[/*] ")
	(orgstruct-mode)
)

(add-hook 'rust-mode-hook 'ngender-rust)

;; ** provide

(provide 'ngender-rust)
