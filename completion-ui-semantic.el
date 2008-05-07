
(defun semantic-prefix-wrapper ()
  "Return prefix at point that Semantic would complete."
  (when (semantic-idle-summary-useful-context-p)
    (let ((prefix (semantic-ctxt-current-symbol (point))))
      (setq prefix (nth (1- (length prefix)) prefix))
      (set-text-properties 0 (length prefix) nil prefix)
      prefix))
)


(defun semantic-completion-wrapper (prefix maxnum)
  "Return list of Semantic completions for PREFIX at point.
Argument MAXNUM is the maximum number of ."
  (when (semantic-idle-summary-useful-context-p)
    (let* (
	   ;; Don't go loading in oodles of header libraries in
	   ;; for minor completions
	   ;; @todo - Don't do this iff the user pressed a key
	   ;;         indicating they don't mind the longer wait.
	   (semanticdb-find-default-throttle
	    (if (featurep 'semanticdb-find)
		(remq 'unloaded semanticdb-find-default-throttle)
	      nil))

	   (ctxt (semantic-analyze-current-context))
	   (acomp (semantic-analyze-possible-completions ctxt)))
      (when (> (length acomp) maxnum)
	(setq acomp (subseq acomp 0 (1- maxnum))))
      (mapcar (lambda (a)
		(substring (semantic-tag-name a) (length prefix)))
	      acomp))
    ))


(defun completion-setup-semantic ()
  "Setup Semantic completion-UI support."
  (interactive)
  (setq completion-function 'semantic-completion-wrapper)
  (setq completion-prefix-function 'semantic-prefix-wrapper)
)
