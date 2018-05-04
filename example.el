;;; ngender-example.el --- template for NGender Emacs Module -*- lexical-binding: t; -*-
 
;; Copyright (C) 2018 J. Greg Davidson
;; Author: J. Greg Davidson <jgd@ngender.net>
;; Maintainer: Someone Else <someone@example.com>
;; Created: 19 Jul 2018
;; Version: 1.0
;; Keywords: languages
;; - only for keywords for the finder-by-keyword help command
;; Homepage: https://ngender.org/RPTUG
;; - we need to make sure that /RPTUG exists as an "alias"
;; Package-Requires: ((gnus "1.0") (bubbles "2.7.2") cl-lib (seq))

;; This file is not part of GNU Emacs.

;; This file is free software.  It may be used under the
;; license specified in the file LICENSE.md which must accompany this file.
;; - What is the new simplified way of referring to licenses with urls?

;;; Commentary:

;; This file is intended as a starting point for creating an
;; NGender Emacs Module.

;; Status: Draft

;; Roadmap: Provide a structure to  meet 95% of NGender Emacs Module needs.
;; - Consider using packages seq and map to simplify set and map processing!!

;; An NGender Emacs Module (NEM) is intended to be lighter-weight
;; than an Emacs Package.  Most commonly, a NEM wraps an Emacs
;; Package, ensuring that the desired package is available, loaded
;; and appropriately configured and activated.  The intention is
;; that the user need only add

;; (ngender example)

;; to their personal Emacs init file in order to have an extension
;; fully in operation.  Many packages have complex configuration
;; options which a user might want to take advantage of.  In such a
;; case, the user should instead specify

;; (ngender example feature...)

;; which currently expands into:

;; (ngender-require 'example '(feature...))

;; ngender-require does nothing if the specified features are already loaded

;; Although an NGender Emacs Module is NOT an Emacs Package,
;; there's value in following applicable Emacs Packaging
;; Conventions as described by
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging-Basics.html

;;; Code:

;; ** Dependencies

;; Ensure that all needed and requested packages have been
;; downloaded and loaded

;; Certain packages need to be loaded to statisfy this
;; module.  The user may request additional or alternative
;; packages.  Users should think in terms of features being
;; required rather than packages being loaded, functions
;; being called, variables being set, etc.  We need to
;; determine what packages support those requirements.

;; Meta-features specify alternative ways to satisfy requirements.
;; Most modules won't need meta-features, but if they do:
(defvar *ngender-example-meta-features*
	'( (feature-name package-name require-name...)... )
	"mapping from feature names to package name and 0 or more require name(s)" )

;; Some packages export exactly one require feature with the same name as the package.
;; Other packages have one or more features with names differing from the package name.
;; Anything managed by a meta-feature should not be in this list!
(defvar *ngender-example-package-requires*
	'( example-mode (fancy-example-package fancy-example-feature-1 fancy-example-feature-2) )
	"mapping from package names to requirement name(s)" )

;; Figure out what meta features, packages and require features we need

(defvar *ngender-example-minimum-features* '(example-mode)
	"minimum set of features requred for this module" )

;; if user doesn't specify this feature, the minimum features become the default
(defvar *ngender-example-features* *ngender-example-minimum-features*
	"features required by the user - must be defined before they load this module" )

;; merge the necessary features into the requested features
(ngender-update-union-with-bags '*ngender-example-features* *ngender-example-minimum-features*)

;; Figure out what packages and require features are implied

(let ( (packages '()) (features '()) )
	
	(apply #'ngender-package packages)
	(apply #'require features)
)

;; ** Provides

(provide 'ngender-example)

‘;;; filename ends here’
