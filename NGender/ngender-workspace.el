;; Extensions and customizations for Project Workspaces

;; A user who is working on multiple Projects or Activities with a
;; single Emacs Process, possibly using a single Emacs Server and
;; multiple Emacs Clients can accumulate a large number of buffers
;; which can make it difficult to focus on the project at hand.

;; Workspaces allow a particular Emacs Frame to be associated with a
;; the of Buffers pertaining to a single Project or Activity such that
;; the User by default only sees those Buffers in that frame.

;; (ngender-package 'persp-mode)
;; (ngender-package 'project-mode
;; (ngender-package 'perspective)

;; ** Provide

(provide 'ngender-workspace)
