;; * Magit Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(require 'ngender)

(defvar *ngender-magit-packages*
	(ngender-symbol-value '*ngender-magit-packages* '(magit))
	"minimal set of magit packages")
(apply #'ngender-package-loaded *ngender-magit-packages*)

;; ** provide

(provide 'ngender-magit)
