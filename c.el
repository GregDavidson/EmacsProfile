;; * C/C++ Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** require

(require 'cc-mode)
;; (ngender cedet)

;; ** Functions

(defconst ngender-c-style
  `(
    (c-tab-always-indent . t)
    (c-comment-only-line-offset . ,ngender-default-indent) ; was 4
    (c-hanging-braces-alist . (
			       (substatement-open after)
			       (brace-list-open) ))
    (c-hanging-colons-alist . (
			       (member-init-intro before)
			       (inher-intro)
			       (case-label after)
			       (label after)
			       (access-label after) ))
    (c-cleanup-list . (
		       scope-operator
		       empty-defun-braces
		       defun-close-semi ))
    (c-offsets-alist . (
			(arglist-close . c-lineup-arglist)
			(substatement-open . 0)
			(case-label . ,ngender-default-indent) ; was 4
			(block-open . 0)
			(knr-argdecl-intro . -) ))
    (c-echo-syntactic-information-p . t)
    (truncate-partial-width-windows . nil)
    ;; semantic bovinator CEDET features:
    ;; (add-to-list 'ac-sources '(ac-source-semantic) nil t)
    ;; (local-set-key (kbd "RET") 'newline-and-indent)
    ;; (linum-mode t)
    ;; (semantic-mode t)
    )
  "C Programming Style for Virtual Infinity Systems" )

;; Customizations for all of c-mode, c++-mode, and objc-mode
(defun ngender-c-mode ()
  (setq
		c-basic-offset ngender-default-indent
		tab-width ngender-default-indent
		indent-tabs-mode t
		c-echo-syntactic-information-p t ;show parse on indent
  )
  (c-add-style "NGENDER" ngender-c-style t)
  ;; offset customizations not in ngender-c-style
  ;; (c-set-offset 'member-init-intro '++)
  ;; other customizations
  ;; we like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)
  ;; keybindings for supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map,
	;; objc-mode-map, java-mode-map, and idl-mode-map
	;; all inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
	(outshine-mode)
)

(add-hook 'c-mode-hook 'ngender-c-mode)
(add-hook 'c++-mode-hook 'ngender-c-mode)

;; ** provide

(ngender-provide c)

;; ** Autoloading

;; (autoload 'ngender-c-mode "ngender-c")
;; (add-to-list 'auto-mode-alist '("\\.[cChH]\\(pp\\|PP\\|xx\\|XX\\|++\\|\\)\\'" . ngender-c-mode)
