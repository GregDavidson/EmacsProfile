;; * Emacs-Lisp Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies - provide and require

(provide 'ngender-elisp)
(require 'ngender)

;; ** Everything Else

(defun ngender-emacs-lisp-mode-hook ()
	(setq orgstruct-heading-prefix-regexp ";;* ")
	(orgstruct-mode)
	(ngender-pitch-mode)
)

(add-hook 'emacs-lisp-mode-hook 'ngender-emacs-lisp-mode-hook)
