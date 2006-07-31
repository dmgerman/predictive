
;;; predictive-setup-latex-ams.el --- predictive mode LaTeX setup function
;;                                    (for AMSmath users)


;; Copyright (C) 2004 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.4
;; Keywords: predictive, setup function, latex

;; This file is part of the Emacs Predictive Completion package.
;;
;; The Emacs Predicive Completion package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Predicive Completion package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Predicive Completion package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Change Log:
;;
;; Version 0.4
;; * modified to work with new auto-overlays package
;;
;; Version 0.3
;; * changed LaTeX commands back to (new) 'word regexps
;; * added comments as 'line regexps
;; * modified priorities and ordering so things work with new switch-dict code
;;
;; Version 0.2
;; * changed 'word regexps to 'start and 'end regexps so that
;;   predictive-learn-from- functions can learn LaTeX commands
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(require 'auto-overlays)
(provide 'predictive-ams-latex)

;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-ams-latex-regexps nil)
(make-local-variable 'predictive-ams-latex-regexps)


(defun predictive-setup-ams-latex ()
  "Sets up predictive mode for use with latex major modes."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-ams-latex)
  (predictive-load-dict 'dict-ams-latex-math)
  (predictive-load-dict 'dict-ams-latex-env)
  (predictive-load-dict 'dict-latex-docclass)

  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-ams-latex-regexps)))
  
  ;; this ensures correct backwards-delete behaviour when \ is involved
  (set (make-local-variable 'words-include-escapes) t)
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-ams-latex-regexps
	(auto-overlay-init
	 '(
	   ;; %'s start comments that last till end of line
	   (line "%" (dict . predictive-main-dict) (priority . 4)
		 (exclusive . t))
	   
	   ;; $'s delimit the start and end of inline maths regions
	   (self "\\$" (dict . dict-ams-latex-math) (priority . 3)
		 (face (background-color . "green")))
	   
	   ;; \begin{ and \end{ start and end LaTeX environments
	   ;; \text{ starts a text region within a maths display
	   ;; \documentclass starts a document
	   ;; All are ended by } but not by \}. The { is included to ensure
	   ;; all { and } match, but \{ is excluded.
	   (stack
	    (start "\\\\begin{" (dict . dict-ams-latex-env) (priority . 2))
	    (start "\\\\end{" (dict . dict-ams-latex-env) (priority . 2))
	    (start "\\\\text{"
		   (dict . (list predictive-main-dict predictive-buffer-dict))
		   (priority . 2))
	    (start "\\\\documentclass[.*?]{"
		   (dict . dict-latex-docclass) (priority . 2))
	    (start ("\\([^\\]\\|^\\)\\({\\)" . 2) (priority . 2))
	    (end ("\\([^\\]\\|^\\)\\(}\\)" . 2) (priority . 2)))
	   
	   ;; \begin{...} and \end{...} start and end various maths displays
	   (stack
	    (start "\\\\begin{equation}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{equation}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{equation\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{equation\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{align}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{align}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{align\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{align\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{alignat}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{alignat}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{alignat\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{alignat\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{flalign}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{flalign}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{flalign\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{flalign\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{gather}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{gather}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{gather\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{gather\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{multline}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{multline}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{multline\\*}"
		   (dict . dict-ams-latex-math) (priority . 1))
	    (end "\\\\end{multline\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)))
	   
	   ;; \ starts a LaTeX command, which consists either entirely of
	   ;; letter characters, or of a single non-letter character
	   (word ("\\\\\\([[:alpha:]]*?\\)\\([^[:alpha:]]\\|$\\)" . 1)
		 (dict . dict-ams-latex))
	   )))
  
  
  ;; make "\", "$", "{" and "}" do the right thing
  (setq predictive-override-syntax-alist
	'((?\\ . (lambda () (interactive)
		   (predictive-abandon)
		   (predictive-insert-and-complete)))
	  (?{ . (lambda () (interactive)
		  (predictive-accept-and-insert)
		  (when (auto-overlays-at-point nil '((identity auto-overlay)
						      (identity dict)))
		    (predictive-complete ""))))
	  (?} . predictive-accept-and-insert)
	  (?\" . (lambda () (interactive)
		   (predictive-accept)
		   (TeX-insert-quote nil)))))

  t  ; indicate succesful setup
)


;;; predictive-ams-latex.el ends here
