;; *** workgroups

;; (setq wg-morph-on nil)

;; *** workgroups2

;; Deleted this package Aug 2 2017 because
;; trying to get persp-mode to work
;; and this this is not compatible
;; Comments with ";; ; " are how it was last

;; package is a bit old - is there something better???
;; ; (require 'workgroups2)

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
;; ; (setq wg-prefix-key (kbd "C-s-RET"))
;; (setq wg-prefix-key (kbd "C-s-<return>"))

;; Change workgroups session file
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
;; ;  (global-set-key (kbd "C-s-<down>") 'wg-save-session)
;; ;  (global-set-key (kbd "C-s-<up>") 'wg-load-session)
;; ;  (global-set-key (kbd "C-M-s-<up>") 'wg-reload-session)
;; ;  (global-set-key (kbd "C-s-<right>") 'wg-switch-to-workgroup)
;; ;  (global-set-key (kbd "C-s-<left>") 'wg-switch-to-previous-workgroup)

