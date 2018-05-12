;; * Font, Face, Indentation Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson
;; 
;; ** Dependencies

;; (require 'x-fonts)					; why??

;; ** Indentation Issues

;; Indentation interacts with fonts when the fonts are proportional -
;; although I see no rational reason why it should be so!

;; Q: Are tabs or spaces better for indentation?

;; A1: TABS, obviously!  Why?  Many reasons!
;; Best reason: Adjusting indentation via tab-width!

;; A2: SPACES, obviously!  Why?  Alignment,
;; flexibility, predictability, reproducibility,
;; transparency - what you see is what you get!

;; Q: Are tabs or spaces better for indentation?
;; A: I guess we better support both!
;; Audit everything for tab/space neutrality/flexibility!!

;; Define desired indentation in space-equivalents
(defconst ngender-min-indent 1)
(defconst ngender-default-indent 2)
(defconst ngender-default-tab-width 2)

(defun ngender-tab-width (n)
  "Set local tab-width interactively to n=0=>default or min(n, min-indent)"
  (interactive "p")
  (make-local-variable 'tab-width)
  (setq tab-width (if (zerop n) ngender-default-tab-width (max n ngender-min-indent))) )

;; ** Font, Face, Pitch Preferences

;; A great deal of research tells us that Variable or Proportional Fonts, where the width of letters varies, e.g. an m is wider than an i, are easier to read.  Nevertheless, many programmers and others who work with 

;; Use a %d format spec where the font size goes, e.g.
;;	"-adobe-courier-medium-r-normal--%d-*-*-*-m-*-iso8859-1"
;; otherwise it will be put at the end as will happen here:
(defvar *ngender-fixed-font* "Dejavu Sans Mono")
(defvar *ngender-variable-font* "Dejavu Sans Condensed")
(defvar *ngender-pitch-mode* :fixed) ; vs. :variable
(defvar *ngender-font*
	(cond
		( (eq *ngender-pitch-mode* :variable ) *ngender-variable-font* )
		( (eq *ngender-pitch-mode* :fixed ) *ngender-fixed-font* ) ) )
(defvar *ngender-frame-font* *ngender-font*)

;; make it easy to switch the default for the current buffer:
(setq buffer-face-mode-face
	(cond
		( (eq *ngender-font* *ngender-variable-font*)	'fixed-pitch)
		( (eq *ngender-font* *ngender-fixed-font*) 'variable-pitch) ) )
;; Switches face to buffer-face-mode-face
(global-set-key "\C-cv" 'buffer-face-mode)

;; ** provide

(ngender-provide font)
