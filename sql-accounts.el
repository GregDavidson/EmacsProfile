;; * Connect SQLi to Database -*- lexical-binding: t; -*-
;; Authors:

;; This is INCOMPLETE!
;; Copy this file to your User-Me directory
;; Remove Read Permission for Group and Others
;; chmod go-r sql-accounts-el
;; Add YOUR account and password information
;; Consider encrypting this file!
;; TO DO: Add Emacs Encryption Info!!

;; ** Dependencies

(ngender sql)

;; function ngender-sql-connect is there, not here!
																				;
;; Loosely following
;; https://github.com/tmtxt/.emacs.d ... tmtxt-sql.el

;; ** Personal sql customization

;; **  sql-connection-alist server info

(defvar sql-connection-alist	'()
	"used by sql-connect and ngender-sql-connect" )

(setq sql-connection-alist
	(let* ( (user "YOUR-DEFAULT-NAME")
					(user_local (concat user ".local"))
					(REMOTE_org (concat "/ssh:" user "@REMOTE.org:"))
					(REMOTE_office (concat user "@REMOTE.office"))
				)
		`(
			 (,user_local
				 (sql-product 'postgres)
				 (sql-port 5432)
				 (sql-server "localhost")
				 (sql-default-directory "")
				 (sql-user ,user)
				 (sql-password "")
				 (sql-database ,user) )
			 ("DB-NAME.local"
				 (sql-product 'postgres)
				 (sql-port 5432)
				 (sql-server "localhost")
				 (sql-default-directory "")
				 (sql-user ,user)
				 (sql-password "")
				 (sql-database "DB-NAME") )
			 ("DB-NAME.local"
				 (sql-product 'mysql)
				 (sql-port 3306)
				 (sql-server "localhost")
				 (sql-default-directory "")
				 (sql-user "USER-NAME")
				 (sql-password "PASSWORD")
				 (sql-database "DB-NAME") )
			 ("DB-NAME.REMOTE"
				 (sql-product 'mysql)
				 (sql-port 3306)
				 (sql-server "localhost")
				 (sql-default-directory ,REMOTE_org)
				 (sql-user "USER-NAME")
				 (sql-password "PASSWORD")
				 (sql-database "DB-NAME") )
			 ("DB-NAME.office"
				 (sql-product 'mysql)
				 (sql-port 3306)
				 (sql-server ,REMOTE_office)
				 (sql-default-directory "")
				 (sql-user "USER-NAME")
				 (sql-password "PASSWORD")
				 (sql-database "DB-NAME") ) ) ) )

;; ** Provides

(ngender-provide sql-accounts)
