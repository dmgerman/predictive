


(require 'predictive)
(require 'auto-overlays)
(require 'predictive-semantic)

(provide 'predictive-elisp)



;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-elisp-regexps nil)
(make-local-variable 'predictive-ams-latex-regexps)



(defun predictive-setup-elisp ()
  "Set up predictive mode for use with emacs lisp major modes."
  (interactive)

  ;; load the elisp dictionary
  (predictive-load-dict 'dict-elisp)
  
  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-elisp-regexps)))

  ;; use emacs lisp dictionary
  (set (make-local-variable 'predictive-main-dict) dict-elisp)
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-elisp-regexps
	(auto-overlay-init
	 ;; "(let" and "(defun" create new scopes
	 '((stack
	    (start "(let\\b"
		   (dict . (:generate
			    predictive-semantic-generate-dict
			    :refresh
			    predictive-semantic-refresh-dict)))
	    (start "(defun\\b"
		   (dict . (:generate
			    predictive-semantic-generate-dict
			    :refresh
			    predictive-semantic-refresh-dict)))
	    (start "(")
	    (end ")")))))
)
