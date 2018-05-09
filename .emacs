;; GNU-Emacs User Profile, typically ~/.emacs
;; I keep the real copy of this under *ngender-emacs-home*
;; and simply symlink it to ~ or *ngender-user-home*

;; Change these paths if appropriate!

(defvar *ngender-user-home* (expand-file-name "~")
	"where you keep your customizations: your home or profile directory" )

(defvar *ngender-emacs-home*
	(expand-file-name ".emacs.d" *ngender-user-home*)
	"where you keep your Emacs customizations" )
;; What has Emacs treat this as its top-level init dir??

(package-initialize)

(defvar *ngender-user-me*
	(expand-file-name "User-Me" *ngender-emacs-home*)
	"where you keep your personal Emacs customizations" )

;; Ensure *ngender-user-me* directory exists
(unless (file-directory-p *ngender-user-me*)
	(make-directory *ngender-user-me*) )

;; Your custom-file holds the customizations from the Emacs
;; Customization System.  Keep it with your personal Emacs
;; customizations.
(setq custom-file
	(expand-file-name "custom-file.el" *ngender-user-me*) )

;; Create the custom-file as an empty file if it doesn't exist
(unless (file-exists-p custom-file)
	(with-temp-file custom-file) )

(load custom-file)

;; If you have an additional customization file, load it
(load (expand-file-name ".gnu-emacs" *ngender-user-home*) t t)

(defvar *ngender-modules-dir*
	(let ( (dir (expand-file-name "NGender" *ngender-emacs-home*)) )
		(if (file-directory-p dir)
			dir
			(lwarn "Expected personal directory %s" dir) nil ) )
	"where you keep your personal Emacs customizations" )

;; load our essential ngender features
(load (expand-file-name "ngender" *ngender-modules-dir*))

;; init.el should orchestrate everything else
(load (expand-file-name "init" *ngender-emacs-home*))
