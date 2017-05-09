;;; Following https://github.com/tmtxt/.emacs.d ... tmtxt-sql.el

;;; default PostgreSQL login params
(setq sql-postgres-login-params
	'( (user :default "greg")
		 (database :default "wicci1")
		 (server :default "localhost")
		 (port :default 5432) ) )

;;; default MySQL login params
(setq sql-mysql-login-params
	'( (user :default "phpmyadmin")
		 (database :default "tiki")
		 (server :default "localhost")
		 (port :default 3306) ) )

;;; hooks
(add-hook 'sql-interactive-mode-hook
	(lambda ()
		(toggle-truncate-lines t)
		(setq-local show-trailing-whitespace nil)
		(auto-complete-mode t) ) )

(add-hook 'sql-mode-hook
	(lambda ()
		(setq-local ac-ignore-case t)
		(auto-complete-mode) ) )

;;; server list
(setq sql-connection-alist
	'(
		 (greg.local
			 (sql-product 'postgres)
			 (sql-port 5432)
			 (sql-server "localhost")
			 (sql-user "greg")
			 (sql-database "greg") )
		 (wicci.local
			 (sql-product 'postgres)
			 (sql-port 5432)
			 (sql-server "localhost")
			 (sql-user "greg")
			 (sql-database "wicci1") )
		 (tiki.local
			 (sql-product 'mysql)
			 (sql-port 3306)
			 (sql-server "localhost")
			 (sql-user "phpmyadmin")
			 (sql-database "tiki") )
		 (tiki.ngender
			 (sql-product 'mysql)
			 (sql-port 3306)
			 (sql-server "ngender.org")
			 (sql-user "phpmyadmin")
			 (sql-database "tiki") )
		 (tiki.office
			 (sql-product 'mysql)
			 (sql-port 3306)
			 (sql-server "ngender-org.office")
			 (sql-user "phpmyadmin")
			 (sql-database "tiki") ) ) )

;;; TODO update this function
(defun jgd-sql-connect (connection)
  "Connect to the input server using sql-connection-alist"
  (interactive
   (helm-comp-read "Select server: "
		 (mapcar (lambda (item)
							 (list
								 (symbol-name (nth 0 item))
								 (nth 0 item)))
			 sql-connection-alist ) ) )
  ;; password
;  (require 'my-sql-pw "jgd-sql-pw.el.gpg")
  (require 'my-sql-pw "jgd-sql-pw.el")
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ((connection-info (assoc connection sql-connection-alist))
         (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info))))
         (sql-password (nth 1 (assoc connection my-sql-pw))))
    ;; delete the connection info from the sql-connection-alist
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    ;; delete the old password from the connection-info
    (setq connection-info (assq-delete-all 'sql-password connection-info))
    ;; add the password to the connection-info
    (nconc connection-info `((sql-password ,sql-password)))
    ;; add back the connection info to the beginning of sql-connection-alist
    ;; (last used server will appear first for the next prompt)
    (add-to-list 'sql-connection-alist connection-info)
    ;; override the sql-product by the product of this connection
    (setq sql-product connection-product)
    ;; connect
    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection))))

(provide 'jgd-sql-connect)
