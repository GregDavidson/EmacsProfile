;; * Magit Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(defvar *ngender-magit-packages* '(magit)
	"minimal set of magit packages" )
(apply #'ngender-package-function *ngender-magit-packages*)

(require 'magit)

;; ** Key Bindings

(global-set-key (kbd "C-x g") 'magit-status)

;; ** provide

(ngender-provide magit)
