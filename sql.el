;; * Fancy SQL Editing Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** loading

;; (autoload 'ngender-sql-mode "ngender-sql")
;; (add-to-list 'auto-mode-alist '("\\.sql\\'" . ngender-sql-mode)

;; Consider moving this into a dependent file
;; and having a new ngender-sql include this
;; and also ngender-sql-connect!!

;; ** Dependencies

;; Consider adding to your init-me.el some of:
;; (defvar *ngender-sql-packages* '( sql sql-indent sqlup-mode ))

(defvar *ngender-sql-packages-necessary* '(sql helm)
	"sql packages necessary for this module to work" )
(defvar *ngender-sql-packages* *ngender-sql-packages-necessary*
	"sql packages necessary for this module and required by user" )
(ngender-update-union-with-bags '*ngender-sql-packages*  *ngender-sql-packages-necessary*)
(apply #'ngender-package-function *ngender-sql-packages*)


(ngender-package rx sql helm)
(require 'rx)
(require 'sql)
(require 'helm)
;; (require 'sqlup-mode)
;; (require 'sql-indent)

;; ** Let's make rx regular expressions extensible

(defun def-rx-1 (name &rest defs)
	(or (assq name rx-constituents)
		(let ( (new-def (cons name (apply #'concat defs))) )
			(push new-def rx-constituents)
			new-def) ) )

(defmacro def-rx (name &rest defs)
	`(def-rx-1 ',name (rx ,@defs)) )

;; ** Now let's get some expressions to make parsing sql easier

(def-rx int (opt (any "+-"))(+ digit))
(def-rx hs space)									; hs = horizontal space
(def-rx hs* (* hs))			; hs* = any (0 or more chars of) hs
(def-rx hs*w hs* word-start)				; hs*w = hs* then a word
(def-rx whs* word-end hs*)	 ; whs* = a word then hs*
(def-rx whs*w whs* word-start)			; whs*w = hs* between words
(def-rx hs+ (+ hs))							; hs+ = some (1 or more chars of) hs
(def-rx s (any space "\n"))							; s = space (perhaps a newline)
(def-rx s* (* s))						; s* = any (0 or more chars of) s
(def-rx s*w s* word-start)		; s*w = s then a word
(def-rx ws* word-end s*)	 ; ws* = a word then s*
(def-rx ws*w ws* word-start)		; ws*w = s* between words
(def-rx s+ (+ s))						; some (1 or more chars of) s
(def-rx name (char alpha "_") (* (char alnum "_"))) ; w/o schema
(def-rx sql-name (opt name ".") name)		; possible schema-qualifier
(def-rx sql-type sql-name (opt "[]")) ; [] suffix for array type names
(def-rx not-rpar (not-char ")"))								; any char not a ")"
(def-rx sql-create-			; beginning of an SQL CREATE of something
				 point hs*w "CREATE" (opt s+ "OR" s+ "REPLACE") s+)
(def-rx sql-create-function-				 ; beginning of an CREATE FUNCTION
				 sql-create-	"FUNCTION" s+)
(def-rx sql-create-function				; parse the whole CREATE FUNCTION
	sql-create-function-
	(group sql-name)																	 ; the function name = \1
	s* "(" s* (group (*? not-rpar)) s* ")" ; the function parameters = \2
	(group s* (*? (not-char "$")))				 ; pre-body clauses = \3
	s*w "AS" ws*													 ;  AS introduces the body
	(group "$" (* (not-char "$")) "$")	; open super-parenthesis = \4
	(group (*? anything))										; the function body = \5
	(backref 4)													; close super-parenthesis = \4
	s* (group (*? (not-char ";"))) s*	; the rest of the clauses = \6
	";" )

;; ** Now we need some genreal helper functions

(defun list-interpose-accum (lst delim accum)
	 "helper function for list-interpose"
	 (if (null lst)	(nreverse accum)
			 (list-interpose-accum
				(cdr lst) delim (cons (car lst) (cons delim accum)) ) ) )

(defun list-interpose (lst delim)
	 "the elements of lst interposed with delim"
	 (if (atom lst)	lst
			 (list-interpose-accum (cdr lst) delim (list (car lst))) ) )

(defun list-concat (lst)
	 "concatenate a list of strings"
	 (apply #'concat lst) )

(defun str-drop-word (str word)
	"drop given word and adjacent space"
	(let
		( (delim (concat "[[:space:]\n]*\\<" (regexp-quote word) "\\>[[:space:]\n]*")) )
		(list-concat (list-interpose (split-string str delim t) " "))
) )

;; ** Now we need some sql-specific helper functions

(defun sql-args (params-str)
	"return a string of $1,$2,... for params-str"
	(if (string-match (rx bos (* (any " \t\n")) eos) params-str)
		""
		(let	( (n (length (split-string params-str "," t))) )
			(list-concat (list-interpose
				 (mapcar (lambda (x) (format "$%d" x)) (number-sequence 1 n)) "," )) )
) )

(defun sql-params (params-str)
	"return a string of $1,$2,... for params-str"
	(if (string-match (rx bos s* eos) params-str)
		""
		(let	( (n (length (split-string params-str (rx s* "," s*) t))) )
			(list-concat (list-interpose (reverse (sql-args-list n)) ","))
) ) )

(defun sql-param-type (param)
		"return the type name given an sql param description"
		(let*
			( (default (rx s* (or "=" (sequence bow "DEFAULT" eow)) s*))
				(split1 (split-string param default))
				(split2 (split-string (nth 0 split1) (rx s+) t)) )
			(nth (1- (length split2) ) split2)
) )

(defun sql-params-types (params-str)
	"return a string of  param1-type,param2-type,... for params-str"
	(if (string-match (rx bos s+ eos) params-str)
		""
		(let ( (split (split-string params-str (rx s* "," s*) t)) )
			(list-concat	(list-interpose (mapcar #'sql-param-type split) ","))
) ) )

(defun nth-match-region (n &optional matches)
	"Return the nth region of the match list as a cons pair"
	(let*
		( (nn (* 2 n)) (m (or matches (match-data)))
			(start (nth nn m))
			(end (nth (1+ nn) m)) )
		(and start end (cons start end)) ) )

(defun region-text (region)
		"Return the text in the given region of current buffer"
	;; Can use either buffer-substring or buffer-substring-no-properties
	(and region
		(buffer-substring-no-properties (car region) (cdr region)) ) )

(defun nth-match-text (n &optional matches)
	"Return the text in the nth region of the match list, if any"
	(region-text (nth-match-region n)) )

(defun all-match-texts (match-data &optional accum)
	"Return a all matches in the match list as a list of strings."
	(if (< (length match-data) 2)
		(reverse accum)
		(all-match-texts
			(cddr match-data)
			(cons (nth-match-text 0 match-data) accum) ) ) )

(defun replace-region (region &rest strings)
	"Replace contents given region by text of given strings."
	(push-mark)
	(delete-region (car region) (cdr region))
	(set-mark (car region))
	(exchange-point-and-mark)
	(apply #'insert	strings)
	(pop-mark) )

(defun decompose-str (str)
	"Decompose str into a list of lines and fragments"
	(let ( (len (length str)) )
		(cond
			( (zerop len) nil )
			( (= 1 len) (list str) )
			( t (let ( (match (string-match "\n" str 1)) )
						(if match
							(cons (substring str 0 match)
								(decompose-str (substring str match)) )
							(list str) ) ) ) ) ) )

(defun smart-decompose (parts)
	"Disassemble the list of strings into lines and fragments"
	(apply #'append (mapcar #'decompose-str parts)) )

;	should check if 1st char of head is a space ???
; 70 is a bit magic???
(defun smart-compose-fit (width head)
	"will head fit onto a line already width wide?"
	(< 70 (+ width (length head))) )

;; this is such crap!
;; Idea: work with list of fragments in reverse
;;    adding newlines as needed, then reverse & concat
;;    width so far can be computed from the list
;; idea: indicate optional space or newline places
;;    with a special value in the list, maybe a symbol
;; idea: an alternate symbol might mean a word-break
;;    is present so no space required but a newline is O.K.
;; FIX!!!

(defun smart-compose (width parts)
	""
	(if (null parts) ""
		(let* ( (head (car parts)) (len (length head)) (tail (cdr parts)) )
			(cond
				( (string-match "^\n" head)
					(concat head (smart-compose (1- len) tail)) )
				( (smart-compose-fit width head)
					(concat  head (smart-compose (+ width len) tail)) )
				( t (concat head "\n" (smart-compose 0 tail)) ) ) ) ) )
	
(defun smart-concat (&rest parts)
	"Concatenate parts with whitespace using
newlines when parts would otherwise wrap
in a narrow window.  Existing indents should
be preserved.  Current implementation is
a placeholder for a correct one!!! "
	(smart-compose 0 (smart-decompose parts))
)

;; ** here are the sql magic interactive functions

(defun beginning-of-sql-function ()
	"Move to 1st line of sql function we are in or after."
	(interactive)
	(beginning-of-line)
	(while (not (or (bobp) (looking-at (rx sql-create-function-))))
		(forward-line -1) ) )

(defun previous-sql-function ()
	"Move to 1st line of previous sql function."
	(interactive)
	(forward-line -1)
	(beginning-of-sql-function) )

(defun next-sql-function ()
	"Move to 1st line of next sql function."
	(interactive)
	(beginning-of-line)
	(while (not (or
								(progn (forward-line) (eobp))
								(looking-at (rx sql-create-function-)) ))) )

(defun match-sql-function ()
	"Goto 1st line of and parse sql function we are in or after"
	(interactive)
	(beginning-of-sql-function)
	(if (looking-at (rx sql-create-function))
		(match-data) ) )

;; Bugs to fix!!!
;; I only seem to be able to handle 7 arguments or parameters!!!
;; strict should be removed from pre-body-str or tail
;; language should be changed to sql in pre-body-str or tail
(defun create-sql-function-pair ()
	"Given an sql function definition conforming to the syntax
	described above in (def-rx sql-create-function ...):
	(1) split it into two,
	(2a) give the first version a 'try_' prefix,
	(2b) make sure	that it is 'STRICT'.
  (3a) replace the body of the second copy with a call to the
	first function wrapped in non_null,
	(3b) make sure that it is NOT 'STRICT'."
	(interactive)
			(let* ( (case-fold-search t) (matches (match-sql-function)) )
				(if matches
					(let*
						( (intro "CREATE OR REPLACE\nFUNCTION ")
							(name (nth-match-text 1 matches))
							(params-str (nth-match-text 2 matches))
							(pre-body-str (nth-match-text 3 matches))
							(super-quote (nth-match-text 4 matches))
							(head (concat name "(" params-str ") "
											pre-body-str " AS " super-quote) )
							(body-region (nth-match-region 5 matches))
							(body (nth-match-text 5 matches))
							(tail-region (nth-match-region 6 matches))
							(tail (str-drop-word (nth-match-text 6 matches) "STRICT"))
							(in1 "\n\t")
							(in2 (concat in1 "\t")) )
						(push-mark)							; mark at top of original function
						(replace-region tail-region tail) ; drop STRICT from the tail
						(replace-region body-region ; replace body with non_null call
							in1 "SELECT non_null("
							in2 "try_"	name "(" (sql-args params-str) "),"
							in2 "'" name "(" (sql-params-types params-str) ")'"
							in1 ")\n" )
						(exchange-point-and-mark)	; goto top of original function
						; new STRICT try_ function with original function's body
						(insert intro "try_" head body super-quote
							" " tail " STRICT;\n\n" )
						(pop-mark)									; OK, we're done!
)) ) )

;; Need support for emitting strings with optional line breaks
;; depending on how wide each chunk of the string is.  The
;; tricky part is that the string may have embedded newlines
;; and tabs!!!

(defun create-sql-function-comment ()
	"Given an sql function definition conforming to the syntax
	described above in (def-rx sql-create-function ...):
	Create a comment command following it and leave
	point in the comment string."
	(interactive)
			(let* ( (case-fold-search t) (matches (match-sql-function)) )
				(if matches
					(let*
						( (region (nth-match-region 0 matches))
							(name (nth-match-text 1 matches))
							(params-str (nth-match-text 2 matches))
							(part1 "\n\nCOMMENT ON FUNCTION")
							(part2 (concat name "(" params-str ")\nIS '" ))
							(part3 "';")	)
						(push-mark)
						(set-mark (cdr region))
						(exchange-point-and-mark)
						(insert (smart-concat part1	" " part2))
						(set-mark (point-marker))
						(save-excursion (insert part3))
						(pop-mark)
 ) ) ) )

;; ** Simpler SQL Setup

(defun sql-startup ()
  "Start a sqli session and set sql-buffer"
  (interactive)
  (sql-product-interactive)
  (sql-set-sqli-buffer-generally)
	(setq buffer-face-mode-face '(:family "DejaVu Sans Mono"))
	(buffer-face-mode)
)

; (global-set-key [?\s-s ?\s-q ?\s-l] 'sql-startup )

;; SQL-Mode is a bit stupid in that it won't try to find the
;; SQLi buffer, even though there will usually only be one
;; and SQL-Mode provides the perfect tool to find it,
;; (sql-find-sqli-buffer), forcing the user to manually set
;; it for each SQL buffer!
(defun ngender-set-sqli-buffer ()
  "Set the SQLi buffer SQL strings are sent to."
  (interactive)
  (let ((default-buffer (sql-find-sqli-buffer)))
    (if (null default-buffer)
	(message "There is no suitable SQLi buffer")
      (setq sql-buffer default-buffer) )
					;(run-hooks 'sql-set-sqli-hook)
) )

(defun sql-outline-minor-mode ()
	"Add outline-minor-mode to sql-mode."
	(interactive)
	(setq outline-minor-mode-prefix "\C-c\C-o")
	(outline-minor-mode 1)
	(setq variable-pitch-mode 1)
	(setq outline-regexp "-- [*\f]+")
)

(defun ngender-bind-sql-magic-functions ()
	"Add magic functions to the sql-mode-map."
	(interactive)
	(define-key sql-mode-map (kbd "C-c M-b")
		'begininning-of-sql-function )
	(define-key sql-mode-map (kbd "C-c M-p") 'previous-sql-function)
	(define-key sql-mode-map (kbd "C-c M-n") 'next-sql-function)
	(define-key sql-mode-map (kbd "C-c M-2") 'create-sql-function-pair)
	(define-key sql-mode-map (kbd "C-c M-t") 'create-sql-function-pair)
	(define-key sql-mode-map (kbd "C-c M-;")
		'create-sql-function-comment )
)

(defun ngender-sql-misc ()
	"Miscellaneous sql-mode customizations."
	(interactive)
	(set-variable 'fill-column 60 t)
)

(defun ngender-sql-mode ()
	(ngender-tab-width 0)
	(ngender-pitch-mode)
	(setq orgstruct-heading-prefix-regexp "-- ") ; maybe /* as well?
	(orgstruct-mode)
	(ngender-set-sqli-buffer)
	(ngender-bind-sql-magic-functions)
)
(add-hook 'sql-mode-hook 'ngender-sql-mode)

;; ** Function ngender-sql-connect

(defun ngender-sql-connect (key)
  "Prompt and Connect using sql-connection-alist, set sql-product"
  (interactive
   (helm-comp-read "Select server: "
		 (mapcar (lambda (item) (let ((name (cl-first item))) (list name name)))
			 sql-connection-alist ) ) )
  ;; get the sql connection info and product from the sql-connection-alist
  (let* ( (info (assoc key sql-connection-alist))
					(prod (cl-second (assoc 'sql-product info))) ; might be (quote product)
					(product (if (consp prod) (cl-second prod) prod)) )
    ;; move found info to front of sql-connection-alist
    (setq sql-connection-alist
			(cons info (assq-delete-all key sql-connection-alist)) )
    ;; override sql-product by the found product
    (setq sql-product product)		 ; <--- side-effect!
    ;; sql-connect will fetch info again using key
    (if current-prefix-arg
      (sql-connect key key)
      (sql-connect key) ) ) )

;; ** provide

(ngender-provide sql)
