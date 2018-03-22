;; * Mozart/Oz Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies - provide and require

(provide 'ngender-mozart)
(require 'ngender)

;; ** Everything Else

;; (when (not (boundp 'ozhome))
;;   (defvar ozhome (or (getenv "OZHOME") (expand-file-name "/usr/lib/mozart")) )
;;   (defvar oz-bin (expand-file-name "bin" ozhome))
;;   (defvar oz-elisp (expand-file-name "share/elisp" ozhome))
;;   (when (file-directory-p ozhome)
;;     (when (and (file-directory-p oz-bin)
;; 	       (not (member exec-path oz-bin)) )
;;       (nconc exec-path (list oz-bin))
;;       (setenv "PATH" (concat (getenv "PATH") ":" oz-bin)) )
;;   (ngender-prepend-paths 'load-path oz-elisp)
  
;;   (ngender-update-union 'auto-mode-alist
;; 		    '("\\.oz\\'" . oz-mode)
;; 		    '("\\.ozg\\'" . oz-gump-mode) )
  
;;   (autoload 'run-oz "oz" "" t)
;;   (autoload 'oz-mode "oz" "" t)
;;   (autoload 'oz-gump-mode "oz" "" t)
;;   (autoload 'oz-new-buffer "oz" "" t) ) )
