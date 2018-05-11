;; * init.el in Emacs Home
;; Features requiring significant customization
;; are in separate byte-compiled files under
;; - NGender/ --> if likely to be useful to others
;; - User-Me/ --> otherwise

;; Everything after one or more semicolons is a comment for
;; humans and is ignored by Emacs.

;; This file should be loaded by ~/.emacs
;; which should be linked to ~/.emacs.d/.emacs
;; See README.org file in this directory

;; This file starts out
;;	a symbolic link from NGender/init.el to .emacs.d
;; If you truly need to change this file, then
;;	(1) remove the link
;;	(2) copy NGender/init.el to User-Me/init.el
;;  (3) create a new link to the copy
;;  (4) edit the copy as needed
;; NOTE: Avoid changing this file if you can accomplish your
;; purpose by editing User-Me/init-me.el

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it.
;; Change any of these paths if needed.

;; Declare any Package (3rd-party library) directories here.
;; "vendor" is a common directory for such.
(when (file-directory-p (expand-file-name "vendor" *ngender-emacs-home*))
				(ngender-emacs-path "vendor") )
				
;; Declare any (Project) Group module directories here, e.g.
;; (ngender-group-dir "Group-RPTUG")

;; Your personal customization directory path,
;; *ngender-user-me*, should be set in your .emacs file
(ngender-user-dir (file-name-nondirectory *ngender-user-me*))
;; Your module path will be kept ordered as follows, first to last:
;; (1) Your User Subdirectory (typically named User-Me)
;; (2) Group (Project) Directories, if any
;; (3) The *ngender-modules-dir* (typically named NGender)

;; Now it's time to load your personal customizations:
(when (file-readable-p (expand-file-name "init-me.el" *ngender-user-me*))
	(ngender "init-me") )	 ; possibly loads a compiled version

;; Remember that Emacs will help you better with init problems if you
;; call it with emacs --debug-init

;; Read the README files for guidance and communicate with
;; the RPTUG Emacs Users!
