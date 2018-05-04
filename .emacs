;; GNU-Emacs User Profile, typically ~/.emacs
;; I keep the real copy of this under *ngender-emacs-home*
;; and simply symlink it to ~ or *ngender-user-home*

;; Change these paths if appropriate!

(defvar *ngender-user-home* (expand-file-name "~")
	"where you keep your customizations: your home or profile directory" )

;; your *ngender-emacs-home* goes under your *ngender-user-home*
(defvar *ngender-emacs-home*
	(expand-file-name ".emacs.d" *ngender-user-home*)
	"where you keep your Emacs customizations" )
;; How do we make this the place where packages will be kept???

(package-initialize)

;; Your *ngender-user-me* goes under *ngender-emacs-home*
(defvar *ngender-user-me*
	(expand-file-name "User-Me" *ngender-emacs-home*)
	"where you keep your personal Emacs customizations" )

;; Create *ngender-user-me* if it doesn't exist
(unless (file-directory-p *ngender-user-me*)
	(make-directory *ngender-user-me*) )

;; Your custom-file holds the customizations from the Emacs
;; Customization System.  It belongs under your personal
;; customization directory.
(setq custom-file
	(expand-file-name "custom-file.el" *ngender-user-me*) )

;; Create the custom-file as an empty file if it doesn't exist
(unless (file-exists-p custom-file)
	(with-temp-file custom-file) )				; create as an empty file

(load custom-file)

;; You may have an additional, system-added customization file
(load (expand-file-name ".gnu-emacs" *ngender-user-home*) t t) ; ignore its absence

;; Make sure the NGender subdirectory is on the load-path
(defvar *ngender-modules-dir*
 (let ( (dir (expand-file-name "NGender" *ngender-emacs-home*)) )
  (if (file-directory-p dir)
   dir
   (lwarn "Expected directory %s" dir) nil ) )
 "where you keep your personal Emacs customizations" )

(load (expand-file-name "ngender.el" *ngender-modules-dir*))

; init.el should orchestrate everything else
(load (expand-file-name "init.el" *ngender-emacs-home*))
