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

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it.
;; Change any of these paths if needed.

;; Declare any Vendor (3rd-party library) directories here.
;; "vendor" is a common directory for such.
(when (file-directory-p (expand-file-name "vendor" *ngender-emacs-home*))
				(ngender-vendor-subdirectory "vendor") )
				
;; Declare any (Project) Group directories here, e.g.
;; (ngender-group-subdirectory "Group-RPTUG")

;; Your personal customization directory path,
;; *ngender-user-me*, should be set in your .emacs file
(ngender-user-subdirectory (file-name-nondirectory *ngender-user-me*))
;; The Emacs path will be kept ordered as follows, first to last:
;; (1) Your User Subdirectory
;; (2) Any Group (Project) Directories
;; (3) Any Vendor (3rd party extension) directories
;; (4) Directories for Packages downloaded from Emacs Repositories

;; Now it's time to load your personal customizations:
(when (file-readable-p (expand-file-name "init-me.el" *ngender-user-me*))
	(load "init-me") )									; might be compiled

;; Remember that Emacs will help you better with init problems if you
;; call it with emacs --debug-init

;; You don't want to put much more in this file.  Read the README
;; files for guidance and communicate with the RPTUG Emacs Users.
