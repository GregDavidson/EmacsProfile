;; * PHP Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(ngender-package 'php-mode)

;; ** Everything Else

(defun pear/php-mode-init()
  "Set some buffer-local variables."
  (setq
		case-fold-search t
		indent-tabs-mode t
		c-basic-offset ngender-default-indent
		tab-width ngender-default-indent
		c-echo-syntactic-information-p t ;show parse on indent
  )
	(outshine-mode)
	(ngender-pitch-mode)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
)
(add-hook 'php-mode-hook 'pear/php-mode-init)

;; ** Provides

(ngender-provide php)
