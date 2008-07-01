
;;;=========================================================
;;;                     dabbrevs

(require 'dabbrev)

(defun dabbrev--wrapper (prefix maxnum)
  "Wrapper around `dabbrev--find-all-completions',
to use as a `completion-function'."
  (dabbrev--reset-global-variables)
  (let ((completions (dabbrev--find-all-expansions prefix nil)))
    (when maxnum
      (setq completions
	    (butlast completions (- (length completions) maxnum))))
    completions))

(setq completion-function 'dabbrev--wrapper)


;;;=========================================================
;;;                        etags

(require 'etags)

(defun etags--wrapper (prefix maxnum)
  "Wrapper around a call to `all-completions'
using `tags-complete-tag', to use as a `completion-function'."
  (let ((completions (all-completions prefix 'tags-complete-tag)))
    (when maxnum
      (setq completions
	    (butlast completions (- (length completions) maxnum))))
    completions))

(setq completion-function 'etags--wrapper)
