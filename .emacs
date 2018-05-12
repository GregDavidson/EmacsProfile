;; GNU-Emacs User Profile Start, typically ~/.emacs

;; See README.org in *ngender-home* for the full story

;; This file starts out as a symbolic link to a generic
;; version under NGender/
;; Try to avoid changing this file, but if you must:
;;	(1) remove the link
;;	(2) copy the generic file to User-Me/
;;  (3) create a new link to the copy
;;  (4) edit the copy as needed

;; We're trying to be very generic with the paths but if
;; they're wrong then you'll need to change this file!

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

(defvar *ngender-home*
	(let ( (dir (expand-file-name "NGender" *ngender-emacs-home*)) )
		(if (file-directory-p dir)
			dir
			(lwarn "Expected personal directory %s" dir) nil ) )
	"where you keep your personal Emacs customizations" )

;; load our essential ngender features
(load (expand-file-name "ngender" *ngender-home*))

;; init.el should orchestrate everything else
(load (expand-file-name "init" *ngender-emacs-home*))
