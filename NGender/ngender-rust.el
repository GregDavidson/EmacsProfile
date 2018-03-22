;; * Rust Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies - provide and require

(provide 'ngender-org)
(require 'ngender)

;; ** Everything Else

;; Racer for Rust

;; Set path to racer binary
(setq racer-cmd "/usr/local/bin/racer")

;; Set path to rust src directory
; (setq racer-rust-src-path "~/.rust/src/")
(setq racer-rust-src-path "/usr/local/lib/rustlib/")

;;;; Rust Support
; http://bassam.co/emacs/2015/08/24/rust-with-emacs/

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

(defun rust-mode-ngender ()
	(ngender-pitch-mode)
	(setq orgstruct-heading-prefix-regexp "/[/*] ")
	(orgstruct-mode)
)

(add-hook 'rust-mode-hook 'rust-mode-ngender)
