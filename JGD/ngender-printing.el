;; * Emacs Postscript Printing Support -*- lexical-binding: t; -*-
;; Authors:
;;	lynn = Lynn B. Dobbs

(defun lbd-ps-landscape ()
  "Change to landscape mode in ps"
  (interactive)
  (setq ps-landscape-mode t )
  )

(defun lbd-ps-portrait ()
  "Change to portrait mode in ps"
  (interactive)
  (setq ps-landscape-mode nil )
  )

(global-set-key [?\s-l] 'lbd-ps-landscape ) 
(global-set-key [?\s-p] 'lbd-ps-portrait ) 

;; Provides

(provide 'ngender-printing)
