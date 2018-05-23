;; * init.el in Emacs Home
;; Features requiring significant customization
;; are in separate byte-compiled files under
;; - NGender/ --> if likely to be useful to others
;; - User-Me/ --> otherwise

;; See README.org in *ngender-home* for the full story

;; This file starts out as a symbolic link to a generic
;; version under NGender/
;; Try to avoid changing this file, but if you must:
;;	(1) remove the link
;;	(2) copy the generic file to User-Me/
;;  (3) create a new link to the copy
;;  (4) edit the copy as needed

;; Everything after one or more semicolons is a comment for
;; humans and is ignored by Emacs.

;; This file should be loaded by ~/.emacs
;; which should be linked to ~/.emacs.d/.emacs
;; See README.org file in this directory

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file init-me.el under it.
;; Change any of these paths if needed.

;; Declare any Package (3rd-party library) directories here.
;; "vendor" is a common directory for such.
(ngender-emacs-path vendor user-emacs-directory)
				
;; Declare any (Project) Group module directories here, e.g.
;; (ngender-group-dir "Group-RPTUG")

;; Your personal customization directory path,
;; *ngender-user-me*, should be set in your .emacs file
(ngender-user-dir *ngender-user-me*)
;; Your module path will be kept ordered as follows, first to last:
;; (1) Your User Subdirectory (typically named User-Me)
;; (2) Group (Project) Directories, if any
;; (3) The *ngender-home* (typically named NGender)

;; Load your personal customizations, if they exist
(ngender-load-module "init-me" t t)

;; Remember that Emacs will help you better with init problems if you
;; call it with emacs --debug-init

;; Read the README files for guidance and communicate with
;; the RPTUG Emacs Users!
