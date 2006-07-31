
(require 'dabbrev)

(defun dabbrev--wrapper (prefix maxnum)
  "Wrapper around `dabbrev--find-all-completions'
 to pass to `completion-define-minor-mode'."
  (dabbrev--reset-global-variables)
  (let ((completions (dabbrev--find-all-expansions prefix nil)))
    (when maxnum
      (setq completions
        (butlast completions (- (length completions) maxnum))))
    (mapcar (lambda (word) (substring word (length prefix)))
      completions)))


(completion-define-minor-mode
  dabbrev-completion-mode
  "A simple completion mode based on dabbrev."
  'dabbrev--wrapper
  nil
  " dabbrev")


(defun my-completion-complete ()
  "Complete word at point or indent."
  (interactive)
  (if (or (looking-at "\\>")
          (memq (char-syntax (char-after (1- (point)))) '(?w ?_)))
      (complete-word-at-point)
    (call-interactively 'indent-for-tab-command)))


