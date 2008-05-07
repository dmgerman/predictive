
(defun semantic-prefix-wrapper ()
  "Returns prefix at point that Semantic would complete."
  (let ((prefix (semantic-ctxt-current-symbol (point))))
    (setq prefix (nth (1- (length prefix)) prefix))
    (set-text-properties 0 (length prefix) nil prefix)
    prefix)
)


(defun semantic-completion-wrapper (prefix maxnum)
  "Returns list of Semantic completions for prefix at point."
  (let* ((ctxt (semantic-analyze-current-context))
	 (acomp (semantic-analyze-possible-completions ctxt)))
    (when (> (length acomp) maxnum)
      (setq acomp (subseq acomp 0 (1- maxnum))))
    (mapcar (lambda (a)
	      (substring (semantic-tag-name a) (length prefix)))
	    acomp))
)


(defun completion-setup-semantic ()
  "Setup Semantic completion-UI support."
  (interactive)
  (setq completion-function 'semantic-completion-wrapper)
  (setq completion-prefix-function 'semantic-prefix-wrapper)
)
