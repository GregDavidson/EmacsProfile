;; * Sample Emacs init.el file

;; Everything after one or more semicolons is a comment for
;; humans and is ignored by Emacs.

;; You might want to copy this to init.el so you can
;; change it without messing up this sample, e.g.
;;	cp init-sample.el init.el

;; This file should be loaded by ~/.emacs
;; which should be linked to ~/.emacs.d/.emacs
;; See README.org file in this directory

;; Study the README.org file to understand the purpose of
;; the directory User-Me and the file User-Me/init-me.el
;; (You are expected to create both of these!)
;; Change any of these paths if needed or desired.

;; Change this default if your .emacs.d is somewhere else:
(defvar *ngender-emacs-home* "~/.emacs.d")
;; Then load the ngender extensions:
(load "~/.emacs.d/NGender/ngender" t t)
;; For help on load (or any other function) do: Control-h f load

;; Any vendor, group or project directories should go here, e.g.
;; (ngender-vendor-subdirectory "Bleeding-Edge")
;; (ngender-group-subdirectory "Group-FOO")
(ngender-user-subdirectory "User-Me")
;; The Emacs path will be kept ordered as follows, first to last:
;; (1) User Subdirectories
;; (2) Group (Project) Directories
;; (3) Vendor (3rd party extension) directories
;; (4) Directories for Packages downloaded from Emacs Repositories

;; Remember that Emacs will help you better with init
;; problems if you call it with emacs --debug-init

;; You don't want to put much more in this file.  Read the
;; README files for guidance and communicate with the Emacs
;; Users in the RPTUG!
