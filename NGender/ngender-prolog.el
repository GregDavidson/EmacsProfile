;; * Prolog Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies - provide and require

(require 'ngender)

;; ** Everything Else

;; see http://turing.ubishops.ca/home/bruda/emacs-prolog/
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog"
;; 	"Major mode for editing Prolog programs." t )
;; (autoload 'mercury-mode "prolog"
;; 	"Major mode for editing Mercury programs." t )
;; (defvar prolog-system 'swi)
;; (ngender-update-union 'auto-mode-alist
;; 		  '("\\.pl\\'" . prolog-mode)
;; 		  '("\\.m\\'" . mercury-mode) )

;; ** provide

(provide 'ngender-prolog)
