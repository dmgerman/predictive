

(provide 'log-modification-mode)


(defvar log-modification-checkpoints 10
  "Number of checkpoints to keep.")

(defvar log-modification-checkpoint-interval 100
  "Save a new checkpoint after this number of modifications.")


(defvar log-modification-buffer nil
  "Buffer in which modifications are recorded.")
(make-variable-buffer-local 'log-modification-buffer)

(defvar log-modification-count 0
  "Used to count modifications.")
(make-variable-buffer-local 'log-modification-count)

(defvar log-modification-checkpoint-count 0
  "Used to count checkpoints.")
(make-variable-buffer-local 'log-modification-count)







(define-minor-mode log-modification-mode
  "Toggle log-modification-mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, all buffer modifications are logged in a separate
buffer called \"*<buffer>-modifications*\", where <buffer> is the
name of the buffer in which log-modification-mode is enabled."

  ;; initial value, mode-line indicator, and keymap
  nil " log" nil

  (cond
   ;; if enabling...
   (log-modification-mode
    ;; create log buffer
    (setq log-modification-buffer
	  (get-buffer-create
	   (concat "*" (buffer-name) "-modifications*")))
    (setq log-modification-count 0)
    ;; save initial checkpoint
    (let ((filename (format "%s~0~.%s"
			    (file-name-sans-extension (buffer-file-name))
			    (file-name-extension (buffer-file-name)))))
      (when (or (not (file-exists-p filename))
		(y-or-n-p (format
			   (concat "Overwrite existing file \"%s\" with "
				   "log-modification checkpoint? ")
			   filename)))
	(copy-file (buffer-file-name) filename t)))
    ;; log changes after every buffer modification
    (add-hook 'after-change-functions 'log-modification nil 'local))

   ;; if disabling, remove function from hook and reset variables
   ((null log-modification-mode)
    (remove-hook 'after-change-functions 'log-modification 'local)
    (kill-local-variable log-modification-buffer)
    (kill-local-variable log-modification-count)
    (kill-local-variable log-modification-checkpoint-count)))
)



(defun log-modification (beg end len)
  "Log a buffer modification. Called from `after-change-functions'."

  (cond
   ;; log an insertion
   ((= len 0)
    (let ((str (buffer-substring-no-properties beg end)))
      ;; replace newlines with explicit \n's
      (when (string-match "\n" str)
	(setq str (replace-match "\\\\n" nil nil str)))
      (with-current-buffer log-modification-buffer
	(goto-char (point-max))
	(insert (concat (format "(goto-char %d)" beg)
			" (inesrt " (prin1-to-string str) ")\n")))))

   ;; log a deletion
   ((= beg end)
    (with-current-buffer log-modification-buffer
      (goto-char (point-max))
      (insert (format "(delete-region %d %d)\n" beg (+ beg len))))))


  ;; if we've reached a checkpoint...
  (setq log-modification-count (1+ log-modification-count))
  (when (= 0 (mod log-modification-count
		  log-modification-checkpoint-interval))
    (setq log-modification-checkpoint-count
	  (1+ log-modification-checkpoint-count))
    (let* ((i (/ log-modification-count log-modification-checkpoint-interval))
	   (filename (format "%s~%d~.%s"
			     (file-name-sans-extension (buffer-file-name))
			     i (file-name-extension (buffer-file-name)))))
      ;; save copy of file, promting for confirmation if over-writing
      (when (or (not (file-exists-p filename))
		(y-or-n-p (format
			   (concat "Overwrite existing file \"%s\" with "
				   "log-modification checkpoint? ")
			   filename)))
	(copy-file (buffer-file-name) filename t)
	;; log checkpoint creation
	(with-current-buffer log-modification-buffer
	  (goto-char (point-max))
	  (insert (format "%% checkpoint %d" i)))

	;; if we've exceded maximum number of checkpoints, delete oldest one
	(when (> log-modification-checkpoint-count
		 log-modification-checkpoints)
	  (delete-file
	   (format "%s~%d~.%s"
		   (file-name-sans-extension (buffer-file-name))
		   (- i log-modification-checkpoints)
		   (file-name-extension (buffer-file-name))))
	  ;; log checkpoint deletion
	  (with-current-buffer log-modification-buffer
	    (insert (format " (checkpoint %d expired)"
			    (- i log-modification-checkpoints)))))

	(with-current-buffer log-modification-buffer (insert "\n")))
      ))
)



(defun log-modification-show-buffer ()
  "Display the modification log buffer in another window."
  (interactive)
  (if (not log-modification-mode)
      (error "log-modification-mode is not enabled")
    (switch-to-buffer-other-window log-modification-buffer)
    (goto-char (point-max))
    (other-window -1)))
