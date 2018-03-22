;; * Semantic Bovinator (CEDET) Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

(provide 'ngender-cedet)

;;; Semantic Bovinator (CEDET)

(require 'cedet)
(require 'semantic/tag)
(require 'semantic/lex)
(require 'semantic/sb)
(require 'semantic/bovine/gcc)
(require 'srecode)

;; select which submodes we want to activate
(setq my-semantic-submodes '(
			     global-semantic-mru-bookmark-mode
			     global-semanticdb-minor-mode
			     global-semantic-idle-scheduler-mode
			     global-semantic-stickyfunc-mode
;;			     global-cedet-m3-minor-mode
			     global-semantic-highlight-func-mode
			     global-semanticdb-minor-mode
			     ))

(dolist (submode my-semantic-submodes)
  (add-to-list 'semantic-default-submodes submode) )
 
;; Activate semantic
(semantic-mode 1)
 
;; load contrib library
;; (require 'eassist)
 
;; customisation of modes
(defun alexott/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
	(when (package-installed-p 'eassist)
		(local-set-key "\C-ct" 'eassist-switch-h-cpp)
		(local-set-key "\C-xt" 'eassist-switch-h-cpp)
		(local-set-key "\C-ce" 'eassist-list-methods)
		)
  (local-set-key "\C-c\C-r" 'semantic-symref)
)
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)
 
(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)

(when (fboundp 'cedet-ectag-version-check)
	(when (cedet-ectag-version-check t)
		(semantic-load-enable-primary-ectags-support) ) )
 
;; SRecode
(global-srecode-minor-mode 1)
 
;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)
 
 ;; Setup JAVA....
;; (require 'cedet-java)

;; if this stuff is still wanted, need to
;; append it onto the ngender-c-style list:

; see ngender.el for
; (defconst ngender-c-style
(defconst ngender-c-style-cedet
  `(
		 semantic bovinator CEDET features:
		 (add-to-list 'ac-sources '(ac-source-semantic) nil t)
		 (local-set-key (kbd "RET") 'newline-and-indent)
		 (linum-mode t)
		 (semantic-mode t)
		 )
  "Semantic Bovinator features for NGender C Programming Style" )
