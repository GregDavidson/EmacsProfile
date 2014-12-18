;;; Emacs Init Code
;; NOT byte-compiled
;; Maintained by jgd

;; Attempted workaround for font problem in 24.3.1
 (setq initial-frame-alist '(
	 (font . "Monospace-10")
	 (vertical-scroll-bars . right)
 ))
 (setq default-frame-alist '(
   (font . "Monospace-10")
	 (vertical-scroll-bars . right)
 ))

;;; Packages

(require 'package)
(defvar my-archives
	'( ("marmalade" . "http://marmalade-repo.org/packages/")
		 ("tromey" . "http://tromey.com/elpa/")
		 ("melpa" . "http://melpa.milkbox.net/packages/") ) )
(dolist (a my-archives)	(add-to-list 'package-archives a t))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(
;;                      clojure-mode ; comes with cider
;;                      clojure-test-mode ; comes with cider
;;	cedet
	cider
	multi-term
;;	scala-mode2
)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p) ) )

(require 'cider)
(require 'multi-term)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(setq nrepl-hide-special-buffers t)

(setq cider-repl-popup-stacktraces t)

;; (setq cider-repl-history-file "path/to/file")

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;;; Load-Paths

(dolist (p '("~/Lib/Emacs" "~/.emacs.d/vendor"
	"~/.emacs.d/jgd" "~/.emacs.d/vendor/ensime/elisp/"))
	(add-to-list 'load-path p) )

;;(load-file "~/.emacs.d/cedet/cedet-devel-load.el")
;;(load-file "~/.emacs.d/cedet/contrib/cedet-contrib-load.el")
;;(load "vendor/cedet/cedet-devel-load")
;;(load "vendor/cedet/contrib/cedet-contrib-load")

;;; Ensime

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; Load some more code

;; Report an error if not found, say nothing if all goes well
(load "jgd" nil t)	; my fancy code in ~/.emacs.d/jgd

(load "frame-bufs" nil t)
(frame-bufs-mode t)

;; (require 'felineherd) ;; what is this?

;; (when window-system (speedbar t))

;;; Key Bindings - (Re)Define some keyboard shortcuts

(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-cv" 'buffer-face-mode)

;;; Org Mod

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)

;; Uniquify
(require 'uniquify)

;;; workgroups2

(require 'workgroups2)

;; <prefix> <key>
;; 
;; <prefix> c    - create workgroup
;; <prefix> A    - rename workgroup
;; <prefix> k    - kill workgroup
;; <prefix> v    - switch to workgroup
;; <prefix> C-s  - save session
;; <prefix> C-f  - load session

;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-s-RET"))
;(setq wg-prefix-key (kbd "C-s-<return>"))

;; Change workgroups session file
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
(global-set-key (kbd "C-s-<down>") 'wg-save-session)
(global-set-key (kbd "C-s-<up>") 'wg-load-session)
(global-set-key (kbd "C-M-s-<up>") 'wg-reload-session)
(global-set-key (kbd "C-s-<right>") 'wg-switch-to-workgroup)
(global-set-key (kbd "C-s-<left>") 'wg-switch-to-previous-workgroup)

(workgroups-mode 1)

;; Workaround to hopefully get rid of annoying:
;; Save Error: "Unbalanced parentheses": /home/greg/.emacs.d/semanticdb/!home!greg!Play!Lang!Lisps!ClojureScript!modern-cljs!resources!public!semantic.cache
(global-semantic-show-unmatched-syntax-mode -1)
