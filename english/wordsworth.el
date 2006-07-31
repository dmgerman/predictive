(defun wordsworth ()
  (interactive)
  (require 'ispell)
  (ispell-init-process)
  (let (word pos poss action)
    (forward-line 0)
    (while (/= (line-end-position) (point-max))
      ;; get current word
;;      (search-forward "\"")
;;      (setq pos (point))
;;      (search-forward "\"")
;;      (setq word (buffer-substring pos (1- (point))))
      (setq pos (line-beginning-position))
      (setq word (buffer-substring (line-beginning-position)
				   (line-end-position)))
      (forward-line 0)
      
      ;; spell-check word
      (process-send-string ispell-process "%\n")
      (process-send-string ispell-process (concat "^" word "\n"))
      (while (progn
	       (accept-process-output ispell-process)
	       (not (string= "" (car ispell-filter)))))
      (setq poss (ispell-parse-output (car (cdr ispell-filter))))
      
      ;; if misspelled, give choice of actions
      (if (not (consp poss))
	  (forward-line 1)
	(while
	    (progn
	      (setq action
		    (read-event
		     "(a) accept  (d) delete  (c) capitalise  (u) uppercase\
  (i) ispell"))
	      (not (or (eq action ?a) (eq action ?d) (eq action ?c)
		       (eq action ?u) (eq action ?i)))))
	(cond
	 ((eq action ?d) (kill-line 1))
	 ((eq action ?c) (goto-char pos) (capitalize-word 1) (forward-line 1))
	 ((eq action ?u) (goto-char pos) (upcase-word 1) (forward-line 1))
	 ((eq action ?i) (goto-char pos) (ispell-word t) (forward-line 0))
	 ((eq action ?a) (forward-line 1))))))
  )



(defun delete-dups ()
  (interactive)
  (let (pos prev next)
    (goto-char (point-min))
    (catch 'failed
      (while (/= (line-end-position) (point-max))
	(search-forward "\"")
	(setq pos (point))
	(search-forward "\"")
	(setq next (buffer-substring pos (1- (point))))
	
	(while (not (string= prev next))
	  (setq prev next)
	  (forward-line 1)
	  (when (null (search-forward "\"" nil t))
	    (throw 'failed t))
	(setq pos (point))
	(search-forward "\"")
	(setq next (buffer-substring pos (1- (point)))))
	(goto-char (line-beginning-position))
	(kill-line 1))))
  )



(defun fill-in-weights ()
  (interactive)
  (let (pos word weight)
    (while (/= (line-end-position) (point-max))
      ;; get current word
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
      
      (save-excursion
	(setq weight 0)
	(set-buffer "dict-english.word-list")
	(goto-char (point-min))
	(when (search-forward (concat "\"" word "\"") nil t)
	  (setq weight (read (current-buffer)))))
      
      (when weight (insert " " (prin1-to-string weight)))
      ))
  )


(defun find-deleted-words ()
  (interactive)
  (set-buffer "dict-english.word-list")
  
  (let ((buff (generate-new-buffer "deleted-words"))
	pos word)
    (while (/= (line-end-position) (point-max))
      ;; get current word
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
      
      (save-excursion
	(set-buffer "dict-english-new.word-list")
	(goto-char (point-min))
	(unless (search-forward (concat "\"" word "\"") nil t)
	  (set-buffer buff)
	  (insert word "\n"))))
    
    (switch-to-buffer buff))
  )


(fset 'places
   (lambda (&optional arg) "Keyboard macro." (interactive "p")
   (kmacro-exec-ring-item (quote ([11 11 24 52 98 112 108 97 99 101 115 46 119
   111 114 100 45 108 105 115 116 return 25 24 111] 0 "%d")) arg)))

(fset 'companies
   (lambda (&optional arg) "Keyboard macro." (interactive "p")
   (kmacro-exec-ring-item (quote ([11 11 24 52 98 99 111 109 112 97 110 105
   101 115 46 119 111 114 100 45 108 105 115 116 return 25 24 111] 0 "%d"))
   arg)))

(fset 'names
   (lambda (&optional arg) "Keyboard macro." (interactive "p")
   (kmacro-exec-ring-item (quote ([11 11 24 52 98 110 97 109 101 115 46 119
   111 114 100 45 108 105 115 116 return 25 24 111] 0 "%d")) arg)))



(defun find-prefices ()
  (interactive)
  (let (pos pos1 pos2 word word1 weight weight1
	    (buff (get-buffer-create "prefix-words")))
    (forward-line 0)
    
    (while (/= (line-end-position) (point-max))
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
      (setq weight (read (current-buffer)))
      (setq word1 nil)
      (setq weight1 0)
      
      (while (search-forward (concat "\"" word) nil t)
	(search-backward "\"")
	(forward-char)
	(setq pos1 (point))
	(search-forward "\"")
	(setq pos2 (1- (point)))	
	(setq word1 (buffer-substring pos1 pos2))
	(setq weight1 (read (current-buffer)))
	
	
	(when word1
	  (cond
	   ;; suffix rules
	   ((or (string= (concat word "s") word1)
		(string= (concat word "es") word1)
		(string= (concat word "d") word1)
		(string= (concat word "ed") word1)
		(string= (concat word (substring word -1) "ed") word1)
		(string= (concat word "n") word1)
		(string= (concat word "en") word1)
		(string= (concat word (substring word -1) "en") word1)
		(string= (concat word "y") word1)
		(string= (concat word "ly") word1)
		(string= (concat word "ing") word1)
		(string= (concat word (substring word -1) "ing") word1)
		(string= (concat word "ingly") word1)
		(string= (concat word (substring word -1) "ingly") word1)
		(string= (concat word "ible") word1)
		(string= (concat word "able") word1)
		(string= (concat word "ment") word1)
		(string= (concat word "al") word1)
		(string= (concat word "ally") word1)
		(string= (concat word "able") word1)
		(string= (concat word (substring word -1) "able") word1)
		(string= (concat word "ity") word1)
		(string= (concat word "ability") word1)
		(string= (concat word (substring word -1) "ability") word1)
		(string= (concat word "ibility") word1)
		(string= (concat word (substring word -1) "ibility") word1)
		(string= (concat word "ance") word1)
		(string= (concat word "ancy") word1)
		(string= (concat word "ate") word1)
		(string= (concat word "ately") word1)
		(string= (concat word "ish") word1)
		(string= (concat word "ist") word1)
		(string= (concat word "ive") word1)
		(string= (concat word "ism") word1)
		(string= (concat word "ists") word1)
		(string= (concat word "ous") word1)
		(string= (concat word (substring word -1) "eous") word1)
		(string= (concat word "shire") word1)
		(string= (concat word "ment") word1)
		(string= (concat word "ments") word1)
		(string= (concat word "ness") word1)
		(and (string= (substring word -1) "t")
		     (string= (concat word "ion") word1))
		(string= (concat word "ation") word1)
		(string= (concat word "ative") word1)
		(and (string= (substring word -1) "s")
		     (string= (concat word "ian") word1))
		(and (string= (substring word -1) "a")
		     (string= (concat word "ns") word1))
		(and (string= (substring word -1) "t")
		     (string= (concat word "ic") word1))
		;; 1 character rule
		(= (length word) (1- (length word1)))
		;; proper noun (capitalised) rule
		(predictive-capitalized-p word)
		;; part of phrase rules
		(string=
		 (substring word1 (length word) (1+ (length word))) " ")
		(string=
		 (substring word1 (length word) (1+ (length word))) "-")
		)
	    (save-excursion
	      (set-buffer buff)
	      (if (bolp) (insert "\"" word "\" (\"" word1 "\"")
		(insert " \"" word1 "\""))))
	   
	   ;; vastly different weights rule
	   ((< weight (* weight1 0.5)))

;; 	 ;; similar weights rule
;; 	 ((or (> weight (* weight1 0.8))
;; 	      (< (- weight1 weight) 50))
;; 	  (save-excursion
;; 	    (set-buffer buff)
;; 	    (insert "\"" word "\" \"" word1 "\"\n")))
	 
	   ;; unable to decide ourselves...
	   (t
;; 	    (while (progn
;; 		     (setq action
;; 			   (read-event
;; 			    (concat "Is \"" word "\" a prefix of \"" word1
;; 				    "\"? (y, n)")))
;; 		     (not (or (eq action ?y) (eq action ?n)))))
;; 	    (when (eq action ?y)
	    (save-excursion
	      (set-buffer buff)
	      (if (bolp) (insert "\"" word "\" (\"" word1 "\"")
		(insert " \"" word1 "\"")))
	    (message ""))
	  )))
      
      (save-excursion
	(set-buffer buff)
	(unless (bolp) (insert ")\n")))
      
      (goto-char pos)
      (forward-line 1)
      ))
)



(defun transfer-prefices ()
  (interactive)
  
  (let (pos word word1)
    (forward-line 0)
    
    (while (/= (line-end-position) (point-max))
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))

      (while (search-forward "\"" (line-end-position) t)
	(setq pos (point))
	(search-forward "\"")
	(setq word1 (buffer-substring pos (1- (point))))

	(save-excursion
	  (set-buffer "dict-english.word-list")
	  (goto-char (point-min))
	  (search-forward (concat "\"" word1 "\""))
	  (if (search-forward "(" (line-end-position) 'noerror)
	      (goto-char (line-end-position))
	    (insert " ("))
	  (when (string= (buffer-substring (1- (point)) (point)) ")")
	    (backward-char)
	    (insert " "))
	  (insert "\"" word "\"")
	  (unless (string= (buffer-substring (point) (1+ (point))) ")")
	    (insert ")")))
	)
      
      (forward-line)))
)



(defun boost-prefices ()
  (interactive)

  (let (pos word word1 weight weight1)
    (forward-line 0)
    
    (while (/= (line-end-position) (point-max))
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
      (setq weight (read (current-buffer)))
      
      (while (search-forward "\"" (line-end-position) t)
	(setq pos (point))
	(search-forward "\"")
	(setq word1 (buffer-substring pos (1- (point))))

	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward (concat "^\"" word1 "\""))
	  (setq pos (point))
	  (setq weight1 (read (current-buffer)))
	  (when (< weight1 weight)
	    (kill-sexp -1)
	    (insert (prin1-to-string weight)))))
      
      (forward-line)))
)



(defun boosted-check ()
  (interactive)

  (let (word word1)
    (forward-line 0)
    (search-forward "\"")
    (setq pos (point))
    (search-forward "\"")
    (setq word (buffer-substring pos (1- (point))))
    (search-forward "\"")
    (setq pos (point))
    (search-forward "\"")
    (setq word1 (buffer-substring pos (1- (point))))
    
    (while (or (string= (concat word "s") word1)
	       (string= (concat word "es") word1)
	       (string= (concat word "d") word1)
	       (string= (concat word "ed") word1)
	       (string= (concat word (substring word -1) "ed") word1)
	       (string= (concat word "n") word1)
	       (string= (concat word "en") word1)
	       (string= (concat word (substring word -1) "en") word1)
	       (string= (concat word "y") word1)
	       (string= (concat word "ly") word1)
	       (string= (concat word "ing") word1)
	       (string= (concat word (substring word -1) "ing") word1)
	       (string= (concat word "ible") word1)
	       (string= (concat word "able") word1)
	       (string= (concat word "ment") word1)
	       (string= (concat word "al") word1)
	       (and (string= (substring word -1) "t")
		    (string= (concat word "ion") word1)))
      (forward-line 1)
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word1 (buffer-substring pos (1- (point)))))
    
    (forward-line 0)
    )
)



(defun boost-list (dict)
  (interactive (list (read-dict "Dictionary: ")))
  
  (let (word word1)
    (goto-char (point-min))
    (search-forward "\"")
    (setq pos (point))
    (search-forward "\"")
    (setq word (buffer-substring pos (1- (point))))
;;    (predictive-boost-prefix-weight dict-english word)
    (predictive-boost-prefix-weight dict word)
    
    (while (/= (line-end-position) (point-max))
      (forward-line 1)
      (search-forward "\"")
      (setq pos (point))
      (search-forward "\"")
      (setq word (buffer-substring pos (1- (point))))
;;      (predictive-boost-prefix-weight dict-english word)))
      (predictive-boost-prefix-weight dict word)))
  )


(defun reset-weights ()
  (interactive)

  (forward-line 0)
  (while (not (= (line-end-position) (point-max)))
    (forward-char)
    (search-forward "\"")
    (kill-sexp)
    (unless (= (point) (line-end-position))
      (insert " nil"))
    (forward-line))
)


(defun add-prefix (word)
  (interactive "sWord: ")
  (forward-line 0)
  (while (search-forward (concat "\"" word) (line-end-position) t)
    (if (search-forward "(" (line-end-position) t)
	(insert "\"" word "\" ")
      (goto-char (line-end-position))
      (insert " (\"" word "\")"))
    (forward-line))
)
