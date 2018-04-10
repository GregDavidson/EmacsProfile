;; * Magit Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(require 'ngender)

(defvar *ngender-magit-packages*
	(ngender-symbol-value '*ngender-magit-packages* '(magit))
	"minimal set of magit packages")
(apply #'ngender-package *ngender-magit-packages*)

(require 'magit)

;; ** Key Bindings

(global-set-key (kbd "C-x g") 'magit-status)

;; ** provide

(provide 'ngender-magit)
