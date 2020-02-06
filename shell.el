;; * Support for Bourne-Compatible Shells -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

;; we should check the environment variable SHELL
;; and set up for the appropriate shell!!

;; This module is customizing both interactive use of a shell
;; and also editing shell scripts!!

;; TO DO!!
;; sh-shell should not be hard wired for bash!!!
;; It should be inherit, or set based on $SHELL or shbang!

;; ** Everything Else

(defun ngender-shell-script-mode-hook ()
  (setq-default sh-basic-offset 2)
  (setq-default sh-indentation 2)
	(setq sh-shell "bash")
	(outshine-mode)
	(ngender-pitch-mode)
)

;; how to get outshine to work with our prompts???
(defun ngender-interactive-shell-mode-hook ()
	;; (setq sh-shell "bash") ;; no such feature!!
	(outshine-mode)
	(ngender-pitch-mode)
)

(add-hook 'sh-mode-hook 'ngender-shell-script-mode-hook)
(add-hook 'shell-mode-hook 'ngender-interactive-shell-mode-hook)

;; Put the current directory of a shell in the mode-line

;;; TO DO!!!
;;; Put a space between the dir path and the buffer name
;;; If in an ssh remote shell, put the host name before the dir path

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

(ngender-provide shell)
