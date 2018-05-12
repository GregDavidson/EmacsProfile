;; * Ensime Scala Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Require

(ngender-package 'ensime)
(require 'ensime)

;; ** The Hook

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Provide

(ngender-provide scala)
