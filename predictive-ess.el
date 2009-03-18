

(require 'predictive-latex)


(defun predictive-setup-ess (arg)
  "With a positive ARG, set up predictive mode for use with ESS files.
With a negative ARG, undo these changes. Called when predictive
mode is enabled via entry in `predictive-major-mode-alist'."
  (cond
   ;; enabling
   ((> arg 0)
    ;; R code (?)
    (auto-overlay-load-definition
     'predictive
     `(nested
       :id Rcode
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(<<.*?>>=\\)" . 3)
	:edge start
	(dict . t) (exclusive . t) (priority . 55)
	(face . (background-color . ,predictive-overlay-debug-colour)))
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(@\\)" . 3)
	:edge end
	(dict . t) (exclusive . t) (priority . 55)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    ;; start R code block (?)
    (auto-overlay-load-definition
     'predictive
     `(word
       :id Rcode-open
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(<<.*?>>=\\)" . 3)
	(dict . t) (exclusive . t) (priority . 55)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    ;; \Sexpr{...}
    (auto-overlay-load-definition
     'predictive
     `(word
       :id Sexpr
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\Sexpr{.*\?}\\)" . 3)
	(dict . t) (priority . 45) (exclusive . t)
	(face . (background-color . ,predictive-overlay-debug-colour)))))
    (predictive-setup-latex arg))

   ;; disabling
   (t (predictive-setup-latex arg))))


(defun predictive-ess-enable-hook-function ()
  "Function to add to `predictive-mode-hook'
to enable predictive ESS support when `predictive-mode' is enabled."
  (cond
   ;; enable ESS support
   ((and (buffer-file-name)
         (string= (file-name-extension (buffer-file-name)) "Rnw"))
    (predictive-setup-ess 1))
   ;; enable LaTeX support
   ;; (note: can't use `predictive-major-mode-alist' any more for LaTeX
   ;; because ESS dynamically switches the major-more to and from latex-mode)
   ((or (eq major-mode 'latex-mode)
        (eq major-mode 'LaTeX-mode))
    (predictive-setup-latex 1))))


(defun predictive-ess-disable-hook-function ()
  "Function to add to `predictive-mode-disable-hook'
to disable predictive ESS support when `predictive-mode' is disabled."
  (cond
   ;; disable ESS support
   ((and (buffer-file-name)
         (string= (file-name-extension (buffer-file-name)) "Rnw"))
    (predictive-setup-ess -1))
   ;; disable LaTeX support
   ;; (note: can't use `predictive-major-mode-alist' any more for LaTeX
   ;; because ESS dynamically switches the major-mode to and from latex-mode)
   ((or (eq major-mode 'latex-mode)
        (eq major-mode 'LaTeX-mode))
    (predictive-setup-latex -1))))


(add-hook 'predictive-mode-hook
	  'predictive-ess-disable-hook-function)
(add-hook 'predictive-mode-disable-hook
	  'predictive-ess-enable-hook-function)

;; remove LaTeX entries from `predictive-major-mode-alist'; we use have to use
;; hook functions instead because ESS dynamically switches the major-mode to
;; and from latex-mode.
(setq predictive-major-mode-alist
      (assq-delete-all 'LaTeX-mode predictive-major-mode-alist))
(setq predictive-major-mode-alist
      (assq-delete-all 'latex-mode predictive-major-mode-alist))
