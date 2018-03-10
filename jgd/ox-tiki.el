;;; ox-tiki.el --- Tiki Wiki Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Copyright (C) 2018 J. Greg Davidson
;; Copyright (C) 2012-2018 Free Software Foundation, Inc.

;; Author: Greg Davidson <greg.davidson@gmail.com>
;; Keywords: org, tiki, wiki

;;; Commentary:

;; This package implements a back-end for Emacs Org Exporter
;; for the Tiki-Wiki Markup Language for which see http://tiki.org
;; This code is based on
;; ox-md.el by Nicolas Goaziou <n.goaziou@gmail.com>
;; which was itself based on the `html' back-end.
;; See Org manual for more information.

;;; Code:

(require 'cl-lib)
(require 'ox-html)
(require 'ox-publish)


;;; User-Configurable Variables

(defgroup org-export-tiki nil
  "Options specific to Tiki export back-end."
  :tag "Org Tiki"
  :group 'org-export
  :version "1.1"
  :package-version '(Org . "9.1"))

;; This will be going away, as well as everything
;; else involving \<style\_>
(defcustom org-tiki-headline-style 'atx
  "Style used to format headlines.
This variable can be set to either `atx' or `setext'."
  :group 'org-export-tiki
  :type '(choice
	  (const :tag "Use \"atx\" style" atx)
	  (const :tag "Use \"Setext\" style" setext)))


;;;; Footnotes

(defcustom org-tiki-footnotes-section "%s%s"
  "Format string for the footnotes section.
The first %s placeholder will be replaced with the localized Footnotes section
heading, the second with the contents of the Footnotes section."
 :group 'org-export-tiki
 :type 'string
 :version "26.1"
 :package-version '(Org . "9.0"))

(defcustom org-tiki-footnote-format "<sup>%s</sup>"
  "Format string for the footnote reference.
The %s will be replaced by the footnote reference itself."
  :group 'org-export-tiki
  :type 'string
  :version "26.1"
  :package-version '(Org . "9.0"))


;;; Define Back-End

(org-export-define-derived-backend 'tiki 'html
  :filters-alist '((:filter-parse-tree . org-tiki-separate-elements))
  :menu-entry
  '(?m "Export to Tiki"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-tiki-export-as-tiki a s v)))
	(?m "To file" (lambda (a s v b) (org-tiki-export-to-tiki a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-tiki-export-to-tiki t s v)
		(org-open-file (org-tiki-export-to-tiki nil s v)))))))
  :translate-alist '((bold . org-tiki-bold)
		     (code . org-tiki-monospace)		; tiki!
		     (example-block . org-tiki-example-block)
		     (export-block . org-tiki-export-block)
		     (fixed-width . org-tiki-monospace)		; tiki!
		     (headline . org-tiki-headline)
		     (horizontal-rule . org-tiki-horizontal-rule)
		     (inline-src-block . org-tiki-verbatim)
		     (inner-template . org-tiki-inner-template)
		     (italic . org-tiki-italic)		; tiki!
		     (item . org-tiki-item)
		     (keyword . org-tiki-keyword)
		     (line-break . org-tiki-line-break)
		     (link . org-tiki-link)
		     (node-property . org-tiki-node-property)
		     (paragraph . org-tiki-paragraph)
		     (plain-list . org-tiki-plain-list)
		     (plain-text . org-tiki-plain-text)
		     (property-drawer . org-tiki-property-drawer)
		     (quote-block . org-tiki-quote-block)
		     (section . org-tiki-section)
		     (src-block . org-tiki-example-block)
		     (template . org-tiki-template)
		     (verbatim . org-tiki-verbatim))
  :options-alist
  '((:tiki-footnote-format nil nil org-tiki-footnote-format)
    (:tiki-footnotes-section nil nil org-tiki-footnotes-section)
    (:tiki-headline-style nil nil org-tiki-headline-style)))


;;; Filters

(defun org-tiki-separate-elements (tree _backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are two exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list,

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

Assume BACKEND is `tiki'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
	   0
	 1))))
  ;; Return updated tree.
  tree)



;;; Transcode Functions

;;;; Bold

(defun org-tiki-bold (_bold contents _info)
  "Transcode BOLD object into Tiki format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "__%s__" contents) )						; tiki!

;;;; Code and Verbatim

(defun org-tiki-monospace (code _contents _info)
  "Transcode CODE object into Tiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value code)))
    (format "-+%s+-" value) ) )		; tiki!

(defun org-tiki-verbatim (verbatim _contents _info)
  "Transcode VERBATIM object into Tiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format "~np~%s~/np~" value) ) )		; tiki!

;;;; Example Block, Src Block and export Block

(defun org-tiki-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Tiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-export-format-code-default example-block info))))

(defun org-tiki-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Tiki.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("TIKI"))
      (org-remove-indentation (org-element-property :value export-block))
    ;; Also include HTML export blocks.
    (org-export-with-backend 'html export-block contents info)))


;;;; Headline

(defun org-tiki-headline (headline contents info)
  "Transcode HEADLINE element into Tiki format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :tiki-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
		  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
	(let ((anchor
	       (and (org-tiki--headline-referred-p headline info)
		    (format "<a id=\"%s\"></a>"
			    (or (org-element-property :CUSTOM_ID headline)
				(org-export-get-reference headline info))))))
	  (concat (org-tiki--headline-title style level heading anchor tags)
		  contents)))))))


(defun org-tiki--headline-referred-p (headline info)
  "Non-nil when HEADLINE is being referred to.
INFO is a plist used as a communication channel.  Links and table
of contents can refer to headlines."
  (unless (org-element-property :footnote-section-p headline)
    (or
     ;; Global table of contents includes HEADLINE.
     (and (plist-get info :with-toc)
	  (memq headline
		(org-export-collect-headlines info (plist-get info :with-toc))))
     ;; A local table of contents includes HEADLINE.
     (cl-some
      (lambda (h)
	(let ((section (car (org-element-contents h))))
	  (and
	   (eq 'section (org-element-type section))
	   (org-element-map section 'keyword
	     (lambda (keyword)
	       (when (equal "TOC" (org-element-property :key keyword))
		 (let ((case-fold-search t)
		       (value (org-element-property :value keyword)))
		   (and (string-match-p "\\<headlines\\>" value)
			(let ((n (and
				  (string-match "\\<[0-9]+\\>" value)
				  (string-to-number (match-string 0 value))))
			      (local? (string-match-p "\\<local\\>" value)))
			  (memq headline
				(org-export-collect-headlines
				 info n (and local? keyword))))))))
	     info t))))
      (org-element-lineage headline))
     ;; A link refers internally to HEADLINE.
     (org-element-map (plist-get info :parse-tree) 'link
       (lambda (link)
	 (eq headline
	     (pcase (org-element-property :type link)
	       ((or "custom-id" "id") (org-export-resolve-id-link link info))
	       ("fuzzy" (org-export-resolve-fuzzy-link link info))
	       (_ nil))))
       info t))))

(defun org-tiki--headline-title (style level title &optional anchor tags)
  "Generate a headline title in the preferred Tiki headline style.
STYLE is the preferred style (`atx' or `setext').  LEVEL is the
header level.  TITLE is the headline title.  ANCHOR is the HTML
anchor tag for the section as a string.  TAGS are the tags set on
the section."
  (let ((anchor-lines (and anchor (concat anchor "\n\n"))))
    ;; Use "Setext" style
    (if (and (eq style 'setext) (< level 3))
        (let* ((underline-char (if (= level 1) ?= ?-))
               (underline (concat (make-string (length title) underline-char)
				  "\n")))
          (concat "\n" anchor-lines title tags "\n" underline "\n"))
        ;; Use "Atx" style
        (let ((level-mark (make-string level ?#)))
          (concat "\n" anchor-lines level-mark " " title tags "\n\n")))))

;;;; Horizontal Rule

(defun org-tiki-horizontal-rule (_horizontal-rule _contents _info)
  "Transcode HORIZONTAL-RULE element into Tiki format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "---")


;;;; Italic

(defun org-tiki-italic (_italic contents _info)
  "Transcode ITALIC object into Tiki format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "''%s''" contents))


;;;; Item

(defun org-tiki-item (item contents info)
  "Transcode ITEM element into Tiki format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ( (type (org-element-property :type (org-export-get-parent item)))
					(struct (org-element-property :structure item))
					(bullet (if (eq type 'ordered) "#" "*")) )
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (pcase (org-element-property :checkbox item)
	      (`on "[X] ")
	      (`trans "[-] ")
	      (`off "[ ] ") )
	    (let ( (tag (org-element-property :tag item)) )
	      (and tag (format "__%s:__ "(org-export-data tag info))) )
	    (and contents
				(org-trim (replace-regexp-in-string "^" "    " contents)) ) ) ) )

;;;; Keyword

(defun org-tiki-keyword (keyword contents info)
  "Transcode a KEYWORD element into Tiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (pcase (org-element-property :key keyword)
    ((or "TIKI" "TIKI") (org-element-property :value keyword))
    ("TOC"
     (let ((case-fold-search t)
	   (value (org-element-property :value keyword)))
       (cond
	((string-match-p "\\<headlines\\>" value)
	 (let ((depth (and (string-match "\\<[0-9]+\\>" value)
			   (string-to-number (match-string 0 value))))
	       (local? (string-match-p "\\<local\\>" value)))
	   (org-remove-indentation
	    (org-tiki--build-toc info depth keyword local?)))))))
    (_ (org-export-with-backend 'html keyword contents info))))


;;;; Line Break

(defun org-tiki-line-break (_line-break _contents _info)
  "Transcode LINE-BREAK object into Tiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \n")


;;;; Link

(defun org-tiki-link (link contents info)
  "Transcode LINE-BREAK object into Tiki format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((link-org-files-as-tiki
	 (lambda (raw-path)
	   ;; Treat links to `file.org' as links to `file.tiki'.
	   (if (string= ".org" (downcase (file-name-extension raw-path ".")))
	       (concat (file-name-sans-extension raw-path) ".tiki")
	     raw-path)))
	(type (org-element-property :type link)))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'tiki))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
			     (org-export-resolve-fuzzy-link link info)
			   (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
	  (`plain-text			; External file.
	   (let ((path (funcall link-org-files-as-tiki destination)))
	     (if (not contents) (format "<%s>" path)
	       (format "[%s](%s)" contents path))))
	  (`headline
	   (format
	    "[%s](#%s)"
	    ;; Description.
	    (cond ((org-string-nw-p contents))
		  ((org-export-numbered-headline-p destination info)
		   (mapconcat #'number-to-string
			      (org-export-get-headline-number destination info)
			      "."))
		  (t (org-export-data (org-element-property :title destination)
				      info)))
	    ;; Reference.
	    (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
	  (_
	   (let ((description
		  (or (org-string-nw-p contents)
		      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
	     (when description
	       (format "[%s](#%s)"
		       description
		       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (let ((path (let ((raw-path (org-element-property :path link)))
		    (cond ((not (equal "file" type)) (concat type ":" raw-path))
			  ((not (file-name-absolute-p raw-path)) raw-path)
			  (t (expand-file-name raw-path)))))
	    (caption (org-export-data
		      (org-export-get-caption
		       (org-export-get-parent-element link)) info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
		  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
	       (path
		(cond
		 ((member type '("http" "https" "ftp" "mailto" "irc"))
		  (concat type ":" raw-path))
		 ((string= type "file")
		  (org-export-file-uri (funcall link-org-files-as-tiki raw-path)))
		 (t raw-path))))
	  (if (not contents) (format "<%s>" path)
	    (format "[%s](%s)" contents path)))))))


;;;; Node Property

(defun org-tiki-node-property (node-property _contents _info)
  "Transcode a NODE-PROPERTY element into Tiki syntax.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (format "%s:%s"
          (org-element-property :key node-property)
          (let ((value (org-element-property :value node-property)))
            (if value (concat " " value) ""))))


;;;; Paragraph

(defun org-tiki-paragraph (paragraph contents _info)
  "Transcode PARAGRAPH element into Tiki format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-prefix-p "#" first-object))
	(concat "\\" contents)
      contents)))


;;;; Plain List

(defun org-tiki-plain-list (_plain-list contents _info)
  "Transcode PLAIN-LIST element into Tiki format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-tiki-plain-text (text info)
  "Transcode a TEXT string into Tiki format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; The below series of replacements in `text' is order sensitive.
  ;; Protect `, *, _, and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-tiki-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Property Drawer

(defun org-tiki-property-drawer (_property-drawer contents _info)
  "Transcode a PROPERTY-DRAWER element into Tiki format.
CONTENTS holds the contents of the drawer.  INFO is a plist
holding contextual information."
  (and (org-string-nw-p contents)
       (replace-regexp-in-string "^" "    " contents)))


;;;; Quote Block

(defun org-tiki-quote-block (_quote-block contents _info)
  "Transcode QUOTE-BLOCK element into Tiki format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-tiki-section (_section contents _info)
  "Transcode SECTION element into Tiki format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-tiki--build-toc (info &optional n keyword local)
  "Return a table of contents.

INFO is a plist used as a communication channel.

Optional argument N, when non-nil, is an integer specifying the
depth of the table.

Optional argument KEYWORD specifies the TOC keyword, if any, from
which the table of contents generation has been initiated.

When optional argument LOCAL is non-nil, build a table of
contents according to the current headline."
  (concat
   (unless local
     (let ((style (plist-get info :tiki-headline-style))
	   (title (org-html--translate "Table of Contents" info)))
       (org-tiki--headline-title style 1 title nil)))
   (mapconcat
    (lambda (headline)
      (let* ((indentation
	      (make-string
	       (* 4 (1- (org-export-get-relative-level headline info)))
	       ?\s))
	     (number (format "%d."
			     (org-last
			      (org-export-get-headline-number headline info))))
	     (bullet (concat number (make-string (- 4 (length number)) ?\s)))
	     (title
	      (format "[%s](#%s)"
		      (org-export-data-with-backend
		       (org-export-get-alt-title headline info)
		       (org-export-toc-entry-backend 'tiki)
		       info)
		      (or (org-element-property :CUSTOM_ID headline)
			  (org-export-get-reference headline info))))
	     (tags (and (plist-get info :with-tags)
			(not (eq 'not-in-toc (plist-get info :with-tags)))
			(let ((tags (org-export-get-tags headline info)))
			  (and tags
			       (format ":%s:"
				       (mapconcat #'identity tags ":")))))))
	(concat indentation bullet title tags)))
    (org-export-collect-headlines info n (and local keyword)) "\n")
   "\n"))

(defun org-tiki--footnote-formatted (footnote info)
  "Formats a single footnote entry FOOTNOTE.
FOOTNOTE is a cons cell of the form (number . definition).
INFO is a plist with contextual information."
  (let* ((fn-num (car footnote))
         (fn-text (cdr footnote))
         (fn-format (plist-get info :tiki-footnote-format))
         (fn-anchor (format "fn.%d" fn-num))
         (fn-href (format " href=\"#fnr.%d\"" fn-num))
         (fn-link-to-ref (org-html--anchor fn-anchor fn-num fn-href info)))
    (concat (format fn-format fn-link-to-ref) " " fn-text "\n")))

(defun org-tiki--footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist (cl-loop for (n _type raw) in fn-alist collect
                            (cons n (org-trim (org-export-data raw info)))))
         (headline-style (plist-get info :tiki-headline-style))
         (section-title (org-html--translate "Footnotes" info)))
    (when fn-alist
      (format (plist-get info :tiki-footnotes-section)
              (org-tiki--headline-title headline-style 1 section-title)
              (mapconcat (lambda (fn) (org-tiki--footnote-formatted fn info))
                         fn-alist
                         "\n")))))

(defun org-tiki-inner-template (contents info)
  "Return body of document after converting it to Tiki syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  ;; Make sure CONTENTS is separated from table of contents and
  ;; footnotes with at least a blank line.
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth
       (concat (org-tiki--build-toc info (and (wholenump depth) depth)) "\n")))
   ;; Document contents.
   contents
   "\n"
   ;; Footnotes section.
   (org-tiki--footnote-section info)))

(defun org-tiki-template (contents _info)
  "Return complete document string after Tiki conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)



;;; Interactive function

;;;###autoload
(defun org-tiki-export-as-tiki (&optional async subtreep visible-only)
  "Export current buffer to a Tiki buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org TIKI Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'tiki "*Org TIKI Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-tiki-convert-region-to-tiki ()
  "Assume the current region has Org syntax, and convert it to Tiki.
This can be used in any buffer.  For example, you can write an
itemized list in Org syntax in a Tiki buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'tiki))


;;;###autoload
(defun org-tiki-export-to-tiki (&optional async subtreep visible-only)
  "Export current buffer to a Tiki file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tiki" subtreep)))
    (org-export-to-file 'tiki outfile async subtreep visible-only)))

;;;###autoload
(defun org-tiki-publish-to-tiki (plist filename pub-dir)
  "Publish an org file to Tiki.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'tiki filename ".tiki" plist pub-dir))

(provide 'ox-tiki)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-tiki.el ends here
