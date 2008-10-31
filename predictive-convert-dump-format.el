
(defun predictive-convert-dump-format ()
  "Convert dictionary dump format from version 20 (and earlier)
of the predictive package to version 21 (i.e. from version < 0.17.x
and earlier of the \"predictive.el\" library to version 0.18)."
  (interactive)
  ;; convert format line by line
  (goto-char (point-min))
  (while (not (eobp))
    (unless (eolp) (forward-sexp))
    ;; convert null weight to 0
    (cond
     ((eolp) (insert " 0"))
     ((looking-at "[[:space:]]+[[:digit:]]+\\([[:space:]]\\|$\\)")
      (forward-sexp))
     ((looking-at "[[:space:]]+\\(nil\\)\\([[:space:]]\\|$\\)")
      (replace-match "0" nil nil nil 1)))
    ;; convert prefix list format
    (unless (or (eolp)
		(looking-at "[[:space:]]+(:prefixes[[:space:]]")
		(null (search-forward "(" (line-end-position) t)))
      (backward-char)
      (insert "(:prefixes ")
      (goto-char (line-end-position))
      (insert ")"))
    (forward-line))
  ;; save converted file
  (save-buffer))
