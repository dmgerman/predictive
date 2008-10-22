
(defun predictive-convert-dump-format ()
  "Convert dictionary dump format from version 20 (and earlier)
of the predictive package to version 21 (i.e. from version < 0.17.x
and earlier of the \"predictive.el\" library to version 0.18)."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "(:prefixes (" nil t)
      (error "File %s appears to have already been converted to\
 the new format" (file-name-nondirectory (buffer-file-name)))
    (while (not (eobp))
      (unless (eolp) (forward-sexp))
      (unless (eolp) (forward-sexp))
      (unless (or (eolp) (null (search-forward "(" (line-end-position) t)))
	(backward-char)
	(insert "(:prefixes ")
	(goto-char (line-end-position))
	(insert ")"))
      (forward-line))
    (save-buffer)))
