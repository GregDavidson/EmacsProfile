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

;; your custom-file
;; holds your Emacs Customization System customizations
;; goes under your *ngender-emacs-home*
(setq custom-file
	(expand-file-name "custom-file.el" *ngender-emacs-home*) )
(load custom-file)

;; You may have an additional, system-added customization file
(load (expand-file-name ".gnu-emacs" *ngender-user-home*) t t) ; ignore its absence

;; Make sure the NGender subdirectory is on the load-path!
(let ( (ngender-dir (expand-file-name "NGender" *ngender-emacs-home*)) )
	(if (file-directory-p ngender-dir)
		(setq load-path (delete-dups (cons ngender-dir load-path)))
		(lwarn "Expected directory %s" ngender-dir) ) )

; init.el should orchestrate everything else
(load (expand-file-name "init.el" *ngender-emacs-home*))
