;; * Clojure Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** requires

(defvar *ngender-clojure-packages* '( clojure-mode typed-clojure-mode cider ) "packages that need loading")
(apply #'ngender-package *ngender-clojure-packages*)
(apply #'require *ngender-clojure-packages*)

;; Configure cider

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(setq cider-repl-popup-stacktraces t)

;; (setq cider-repl-history-file "path/to/file")

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; ** Configure Clojure-Nrepl

;; Is this still needed, given Cider??

;; Based in part on the .emacs.d imported by
;; git clone https://github.com/flyingmachine/emacs-for-clojure.git ~/.emacs.d

;; (add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

;; (setq nrepl-history-file "~/.emacs.d/nrepl-history")
;; (setq nrepl-popup-stacktraces t)
;; (setq nrepl-popup-stacktraces-in-repl t)

;; (defun pnh-clojure-mode-eldoc-hook ()
;;   (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
;;   (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
;;   (nrepl-enable-on-existing-clojure-buffers)
;;   ;; prevent hippie expand from trying file names
;;   (dolist (fn 'try-complete-file-name 'try-complete-file-name-partially)
;;     (setq hippie-expand-try-functions-list (delete fn hippie-expand-try-functions-list)) )
;;   (setq ido-use-filename-at-point nil)
;;   )

;; (add-hook 'nrepl-connected-hook 'pnh-clojure-mode-eldoc-hook)

;; Configure Clojure-Mode

(ngender-update-union-with-items 'auto-mode-alist '("\\.cljs\\'" . clojure-mode) )

;; (add-hook 'nrepl-mode-hook 'subword-mode)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'nrepl-mode) )

;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;; ** provide

(ngender-provide ngender-clojure)

;; ** Loading

;; (autoload 'ngender-clojure-mode "ngender-clojure")
;; (add-to-list 'auto-mode-alist '("\\.\\(clj[cxs]\\?\\|dtm\\|edn\\)\\'" . ngender-clojure-mode))
