;; * Common Lisp Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** require

(require 'slime)

;; ** slime-setup

(defun slime-setup-ngender (slime-dir lisp-program)
  "Set up Slime to use the right Common Lisp"
  (ngender-prepend-paths 'load-path slime-dir)
  (when (and (member slime-dir load-path)
 	     (file-exists-p lisp-program) )
     (setq inferior-lisp-program lisp-program)
     (require 'slime)
     (slime-setup) ) )

(defun slime-setup-clisp ()
   (interactive)
   (slime-setup-ngender "~/Lib/Emacs/slime/" "/usr/bin/clisp") )

;; ** provide

(provide 'ngender-clisp)
