;; * Emacs-Lisp Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

;; ** Everything Else

(defun ngender-emacs-lisp-mode-hook ()
	(setq orgstruct-heading-prefix-regexp ";;* ")
	(orgstruct-mode)
	(ngender-pitch-mode)
)

(add-hook 'emacs-lisp-mode-hook 'ngender-emacs-lisp-mode-hook)

;; ** provide

(ngender-provide elisp)
