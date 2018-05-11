;; Module ngender-test

;; Shows that we've been loaded, and with which features.
;; Messages will appear in the Emacs *Messages* buffer.

(dolist (feature (ngender-features ngender-test))
	(message "ngender-test must provide feature %s" feature) )

;; Record that we've been loaded with those features

(ngender-provide ngender-test)
