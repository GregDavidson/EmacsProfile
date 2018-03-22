;; * Exploring Emacs SQL Possibilities -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; It's not yet clear what may emerge from this!

;; ** Dependencies - provide and require

;; (provide 'ngender-sql-test)
;; (require 'ngender)

;; ** Everything Else

(defun sql-func-play ()
	"Given an sql function definition conforming to the syntax
	described above in (def-rx sql-create-function ...):
	pull out some info and put it in the buffer.
	This is a good starting point for doing something
	more constructive!"
	(interactive)
		(save-excursion
			(let* ( (case-fold-search t) (matches (look-at-sql-func)) )
				(if matches (let*
					( (intro "CREATE OR REPLACE\nFUNCTION ")
						(name (nth-match 1 matches))
						(params-str (nth-match 2 matches))
						(pre-body-str (nth-match 3 matches))
						(super-quote (nth-match 4 matches))
						(head (concat name "(" params-str ") " pre-body-str " AS " super-quote))
						(body (nth-match 5 matches))
						(tail (str-drop-word (nth-match 6 matches) "STRICT"))
						(in1 "\n\t")
						(in2 (concat in1 "\t")) )
					(insert
						(format "%S\n" matches)
						(format "%s: %s\n" 'name name)
						(format "%s: %s\n" 'params-str params-str)
						(format "%s: %s\n" 'pre-body-str pre-body-str)
						(format "%s: %s\n" 'super-quote super-quote)
						(format "%s: %s\n" 'head head)
						(format "%s: %s\n" 'body body)
						(format "%s: %s\n" 'tail tail)
						"\n"
) )) ) ) )

(def-rx sql-create-function-to-params
	sql-create-function-
	(group sql-name)																	 ; the function name
	s* "(" s* (group (*? not-rpar)) s* ")" ; the function parameters
)

(defun look-to-params ()
	(interactive)
	(message "%s" (looking-at (rx sql-create-function-to-params))) )

(def-rx sql-create-function-to-at
	sql-create-function-to-params
	s* (group (*? (not-char "$")))				 ; pre-body clauses
	s*w "AS" s*														; the AS clause must be first
)

(defun look-to-at ()
	(interactive)
	(message "%s" (looking-at (rx sql-create-function-to-at))) )

(def-rx sql-create-function-to-body
	sql-create-function-to-at
	(group "$" (* (not-char "$")) "$")	; open super-parenthesis
	(group (*? any))										; the function body
	(backref 4)													; close super-parenthesis
)

(defun look-to-body ()
	(interactive)
	(message "%s" (looking-at (rx sql-create-function-to-body))) )

(defun look-to-end ()
	(interactive)
	(message "%s" (looking-at (rx sql-create-function))) )

(defun looking-at-sql-func ()
	(interactive)
	(beginning-of-sql-function)
	(message "%S" (setq sql-func-matches
			    (when (looking-at (rx sql-create-function)) (match-data) ) )) )

;; (setq f "CREATE OR REPLACE
;; FUNCTION list_strict_non_try_funcs(_schema name)
;; RETURNS SETOF regprocedure AS $$
;; 	SELECT pg_proc.oid::regprocedure
;; 	FROM pg_proc, pg_namespace
;; 	WHERE proisstrict AND NOT (proname LIKE 'try_%')
;; 	AND pronamespace = pg_namespace.oid	AND nspname = $1
;; $$ LANGUAGE sql;")

;; (string-match (rx create-) f)
;; 0

;; (string-match (rx create-function-) f)
;; 0

;; (string-match (rx create-function) f)
;; 0
;; (match-string 0)

;; (string-match (rx create-function) f)
;; 0

;; (match-string 0 f)
;; "CREATE OR REPLACE
;; FUNCTION list_strict_non_try_funcs(_schema name)
;; RETURNS SETOF regprocedure AS $$
;; 	SELECT pg_proc.oid::regprocedure
;; 	FROM pg_proc, pg_namespace
;; 	WHERE proisstrict AND NOT (proname LIKE 'try_%')
;; 	AND pronamespace = pg_namespace.oid	AND nspname = $1
;; $$ LANGUAGE sql;"

;; (match-string 1 f)
;; "list_strict_non_try_funcs"

;; (match-string 2 f)
;; "_schema name"

;; (match-string 3 f)
;; "$$"

;; (match-string 4 f)
;; "
;; 	SELECT pg_proc.oid::regprocedure
;; 	FROM pg_proc, pg_namespace
;; 	WHERE proisstrict AND NOT (proname LIKE 'try_%')
;; 	AND pronamespace = pg_namespace.oid	AND nspname = $1
;; "

;; (match-string 5 f)
;; "LANGUAGE sql"

;; (match-string 6 f)
;; nil

;; (list-concat (list-interpose '("a" "b" "c") ", "))
;; "a, b, c"

;; (list-interpose '("hello") ", ")
;; ("hello")

;; (list-interpose '() ", ")
;; nil

;; (str-drop-word "this silly package" "silly")
;; "this package"

;; (str-drop-word "this silly package" "this")
;; "silly package"

;; (str-drop-word "this silly package" "package")
;; "this silly"

;; (str-drop-word "this silly package" "hello")
;; "this silly package"

;; (list-concat (list-interpose '("hello") ", "))


