;; Let's make quoting expressions to pass to
;; functions like assert_true easier
;; I want to be able to translate among:
;; SELECT inferred_cat_path('LOYL', 'Observer', 'Readable') = 'Project::LOYL::Observer_Readable';
;; CALL assert_true( 'inferred_cat_path(\'LOYL\', \'Observer\', \'Readable\') = \'Project::LOYL::Observer_Readable\'' );
;; when no region is selected, and otherwise between
;; inferred_cat_path('LOYL', 'Observer', 'Readable') = 'Project::LOYL::Observer_Readable'
;; and
;; 'inferred_cat_path(\'LOYL\', \'Observer\', \'Readable\') = \'Project::LOYL::Observer_Readable\''
;; and in the latter case, I want to leave unchanged anything outside the quotes

;; These functions from Xah might help inspire,
;; although I'd rather do more with regexps

(defun xah-escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))
