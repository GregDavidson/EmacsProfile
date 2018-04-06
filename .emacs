;; GNU-Emacs User Profile, typically ~/.emacs
;; I keep the real copy of this under *ngender-emacs-home*
;; and simply symlink it to ~ or *ngender-user-home*

;; (package-initialize)

;; Chnage these paths if appropriate!

(defvar *ngender-user-home* (expand-file-name "~")
	"where you keep your customizations: your home or profile directory" )

;; your *ngender-emacs-home* goes under your *ngender-user-home*
(defvar *ngender-emacs-home*
	(expand-file-name ".emacs.d" *ngender-user-home*)
	"where you keep your Emacs customizations" )

;; your custom-file goes under your *ngender-emacs-home*
(defvar custom-file
	(expand-file-name "custom-file.el" *ngender-emacs-home*)
	"where Emacs Customization System keeps your customizations")
(load custom-file)

;; You may have an additional, system-added customization file
(load (expand-file-name ".gnu-emacs" *ngender-user-home*) t t) ; ignore its absence

; init.el should orchestrate everything else
(load (expand-file-name "init.el" *ngender-emacs-home*))
