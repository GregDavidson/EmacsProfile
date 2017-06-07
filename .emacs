;; GNU-Emacs User Profile [ ~/.emacs ]
;; I prefer to keep everything emacs under .emacs.d/
;; so I symlink this file to ~/.emacs.d/.emacs

;; SuSE has a fallback of "/etc/skel/.gnu-emacs"
;; I may have a trimmed down version of it:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.gnu-emacs" t t)

; A common place to start user customizations
(load "~/.emacs.d/init.el" nil t)

;; Emacs Options & Settings automatically put here:
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file t t)
