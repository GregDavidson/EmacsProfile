;; * Tcl Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(require 'tcl)

;; Customization

(defun ngender-tcl-preferences ()
  (setq tcl-application "tclsh"
	tcl-indent-level ngender-default-indent
	tcl-continued-indent-level ngender-default-indent
	tcl-use-smart-word-finder t)  )

(add-hook 'tcl-mode-hook 'ngender-pitch-mode)

;; Provides

(provide 'ngender-tcl)
