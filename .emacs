;; GNU-Emacs User Profile [ ~/.emacs ]

(if (file-readable-p "~/.gnu-emacs")
	(load "~/.gnu-emacs" nil t) )
; On SuSE, else of last if was:
;; (if (file-readable-p "/etc/skel/.gnu-emacs")
;; 	(load "/etc/skel/.gnu-emacs" nil t))

(if (file-readable-p "~/.emacs.d/init.el")
	(load "~/.emacs.d/init.el" nil t))

;; Custom Settings
;; ===============
;; To avoid any trouble with the customization system of GNU emacs
;; we set the default file ~/.gnu-emacs-custom
(setq custom-file "~/.emacs.d/custom-file.el")
(if (file-readable-p custom-file)
	(load custom-file t t) )
