;; * Haskell Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson
;; 
;; GHC 2.7.0, November 2009
;; Bring up to date!!

;; ** Dependencies

;; ** Everything Else

;; (ngender-prepend-paths 'load-path "~/Lib/Emacs/HaskellMode")

;; (load "haskell-site-file")

;; (ngender-update-union-with-items 'auto-mode-alist
;; 		  '("\\.[hg]s\\'"  . haskell-mode)
;; 		  '("\\.hi\\'"     . haskell-mode)
;; 		  '("\\.l[hg]s\\'" . literate-haskell-mode) )

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'haskell-mode-hook 'font-lock-mode)

;; Declaration scanning: just use M-x imenu or:
(global-set-key [(control meta down-mouse-3)] 'imenu)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; Older Haskell preferences (remove soon):

;; (autoload 'haskell-mode "haskell-mode"
;;   "Major mode for editing Haskell scripts." t)
;; (autoload 'literate-haskell-mode "haskell-mode"
;;   "Major mode for editing literate Haskell scripts." t)

;; adding any of the following lines according to
;; which modules you want to use:

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-font-lock)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)

;; ** provide

(ngender-provide haskell)
