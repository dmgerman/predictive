
;;; Completion-UI source for ispell correction

(require 'flyspell)

(defgroup completion-ui-ispell nil
  "Completion-UI ispell completion source."
  :group 'completion-ui)

(defcustom ispell-correct-completion-sort-corrections t
  "When non-nil, `complete-ispell' will sort ispell suggestions."
  :group 'completion-ui-ispell
  :type 'boolean)



(defun ispell-correct-completion-function (word)
  (let (poss ; possibilities offered by ispell
        ispell-filter)
    ;; Now check spelling of word.
    (ispell-send-string "%\n") ; put in verbose mode
    (ispell-send-string (concat "^" word "\n")) ; lookup the word
    ;; Wait until ispell has processed word.
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (when (consp ispell-filter)
      (setq poss (ispell-parse-output (car ispell-filter))))
    (cond
     ((or (eq poss t) (stringp poss))
      (message "Ispell: %s is correct" word)
      nil)
     ((null poss)
      (error "Ispell: error in Ispell process")
      nil)
     (t
      ;; The word is incorrect, we have to propose replacements
      (if ispell-correct-completion-sort-corrections
	  (sort (car (cdr (cdr poss))) 'string<)
	(car (cdr (cdr poss))))))))


(completion-ui-register-source
 'ispell-correct-completion-function
 :non-prefix-completion t
 :name 'ispell)

;; ----------------------------------------------------------------------------
