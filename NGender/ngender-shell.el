;; * Support for Bourne-Compatible Shells -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(require 'ngender)

;; we should check the environment variable SHELL
;; and set up for the appropriate shell!!

;; ** Everything Else

(defun ngender-sh-mode-hook ()
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
	(setq orgstruct-heading-prefix-regexp "##* ")
	(orgstruct-mode)
	(ngender-pitch-mode)
)

(add-hook 'sh-mode-hook 'ngender-sh-mode-hook) ; this doesn't do it for shell-mode
(add-hook 'shell-mode-hook 'ngender-sh-mode-hook) ; this does

;; Put the current directory of a shell in the mode-line
(defconst ngender-short-dirpath-max 40)
(defun ngender-short-dirpath (&optional path)
	(let ( (d (replace-regexp-in-string "\\`/[^/]*/[^/]*" "~" (expand-file-name (or path ".")))) (last-d nil) )
		(while (or (> (length d) ngender-short-dirpath-max) (equal d last-d))
			(setq last-d d)
			(setq d (replace-regexp-in-string "\\`[~/.]*[^/]*/?" "..." d)) )
		(replace-regexp-in-string "/?\\'" "" d)	) )
(defun add-mode-line-dirtrack ()
	(add-to-list 'mode-line-buffer-identification
;		'(:propertize (" " (:eval (ngender-short-dirpath)) " ") face dired-directory)
		'(:propertize (:eval (ngender-short-dirpath)) face dired-directory)
		))
(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

;; ** provide

(provide 'ngender-shell)
