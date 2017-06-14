;; GNU-Emacs User Profile [ ~/.emacs ]
;; I prefer to keep everything emacs under .emacs.d/
;; so I symlink this file to ~/.emacs.d/.emacs

;; SuSE has a fallback of "/etc/skel/.gnu-emacs"
;; I may have a trimmed down version of it:

;; (package-initialize)

(load "~/.gnu-emacs" nil t)

(load "~/.gnu-emacs" t t)
>>>>>>> 6cae221328c4377eedd681974d7fd7c8f96eafeb

; A common place to start user customizations
(load "~/.emacs.d/init.el" nil t)

;; Emacs Options & Settings automatically put here:
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file t t)
