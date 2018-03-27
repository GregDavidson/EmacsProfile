;; ** Capture-Mode

;; We're capturing everything under our GTD directory
;; Maybe someday we'll separate these things out?

(global-set-key "\C-cc" 'org-capture)

(defconst gtd-dir (expand-file-name "GTD" org-directory))
(defconst gtd-inbox-org (expand-file-name "inbox.org" gtd-dir))
(defconst gtd-gtd-org (expand-file-name "gtd.org" gtd-dir))
(defconst gtd-tickler-org (expand-file-name "tickler.org" gtd-dir))
(defconst gtd-someday-org (expand-file-name "someday.org" gtd-dir))

(defconst gtd-bookmarks-org (expand-file-name "bookmarks.org" gtd-dir))
(defconst gtd-journal-org (expand-file-name "journal.org" gtd-dir))
(defconst gtd-ideas-org (expand-file-name "ideas.org" gtd-dir))
(defconst gtd-misc-org (expand-file-name "misc.org" gtd-dir))

(setq org-agenda-files (list gtd-inbox-org gtd-gtd-org gtd-tickler-org))

(setq org-capture-templates
	`( ( "t" "Todo [inbox]" entry
			 (file+headline ,gtd-inbox-org "Tasks")
			 "* TODO %i%?" )
		 ( "T" "Tickler" entry
			 (file+headline ,gtd-tickler-org "Tickler")
			 "* %i%? \n %U" )
		 ("b" "Bookmarks" entry (file+headline ,gtd-bookmarks-org "Bookmarks")
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("j" "Journal" entry (file+datetree ,gtd-journal-org)
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("i" "Ideas" entry (file+headline ,gtd-ideas-org "Ideas")
			 "* %?\nEntered %U\n  %i\n  %a")
		 ("m" "Misc" entry (file+headline ,gtd-misc-org "Miscellany")
			 "* %?\nEntered %U\n  %i\n  %a\n  %x")
		 ) )

;; There's some confusion about org-refile-targets
;; Initially none of these worked.  Then the first
;; one started working.  I may have needed to put
;; in a couple of levels of headings.
;; I would definitely like more flexibility and/or
;; to be able to file things to a deeper level!

(setq org-refile-targets `((,gtd-gtd-org :maxlevel . 3)
                           (,gtd-someday-org :level . 1)
                           (,gtd-tickler-org :maxlevel . 2)
                           (,gtd-bookmarks-org :level . 1)
                           (,gtd-journal-org :maxlevel . 4)
                           (,gtd-ideas-org :maxlevel . 4)
                           (,gtd-misc-org :maxlevel . 2)
))

;; (setq org-refile-targets `(
;; 			    (nil :maxlevel . 5)
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
                           
;; (setq org-refile-targets `(
;; 			    (,org-agenda-files :maxlevel . 5) ; do I want , here?
;; 			    (,gtd-someday-org :level . 1) ))
                           
