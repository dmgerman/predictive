
;;; predictive-setup-latex.el --- predictive mode LaTeX setup function


;; Copyright (C) 2004 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.5.1
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
;; Version 0.5.1
;; * minor bug fix
;;
;; Version 0.5
;; * modified to work with new auto-overlays package
;;
;; Version 0.4.1
;; * corrected reference to a latex-ams ditionary 
;;
;; Version 0.4
;; * changed LaTeX commands back to (new) 'word regexps
;; * added comments as 'line regexps
;; * modified priorities and ordering so things work with new switch-dict code
;;
;; Version 0.3
;; * changed 'word regexps to'start and 'end regexps so that
;;   predictive-learn-from functions can learn LaTeX commands
;;
;; Version 0.2
;; * updated to take advantage of new dictionary switching features
;;   in predictive package version 0.3.
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(require 'auto-overlays)
(provide 'predictive-latex)

;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-latex-regexps nil)
(make-local-variable 'predictive-latex-regexps)


(defun predictive-setup-latex ()
  "Sets up predictive mode for use with latex major modes."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-latex)
  (predictive-load-dict 'dict-latex-math)
  (predictive-load-dict 'dict-latex-env)
  (predictive-load-dict 'dict-latex-docclass)

  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-latex-regexps)))
  
  ;; this ensures correct backwards-delete behaviour when \ is involved
  (set (make-local-variable 'words-include-escapes) t)
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-latex-regexps
	(auto-overlay-init
	 '(
	   ;; %'s start comments that last till end of line
	   (line "%" (dict . predictive-main-dict) (priority . 4)
		 (exclusive . t))
	 
	   ;; $'s delimit the start and end of inline maths regions
	   (self "\\$" (dict . dict-latex-math) (priority . 3))
	 
	   ;; \begin{ and \end{ start and end LaTeX environments
	   ;; \documentclass starts a document
	   ;; All are ended by } but not by \}. The { is included to ensure
	   ;; all { and } match, but \{ is excluded.
	   (stack
	    (start "\\\\begin{" (dict . dict-latex-env) (priority . 2))
	    (start "\\\\end{" (dict . dict-latex-env) (priority . 2))
	    (start "\\\\documentclass[.*?]{"
		   (dict . dict-latex-docclass) (priority . 2))
	    (start ("\\([^\\]\\|^\\)\\({\\)" . 2) (priority . 2))
	    (end ("\\([^\\]\\|^\\)\\(}\\)" . 2) (priority . 2))
	    (start ("^\\({\\)" . 1) (priority . 2))
	    (start ("[^\\]\\({\\)" . 1) (priority . 2))
	    (end ("^\\(}\\)" . 1) (priority . 2))
	    (end ("[^\\]\\(}\\)" . 1) (priority . 2)))
	   
	   ;; \begin{...} and \end{...} start and end various maths displays
	   (stack
	    (start "\\\\begin{equation}"
		   (dict . dict-latex-math) (priority . 1))
	    (end "\\\\end{equation}"
		 (dict . dict-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{equation\\*}"
		   (dict . dict-latex-math) (priority . 1))
	    (end "\\\\end{equation\\*}"
		 (dict . dict-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{eqnarray}"
		   (dict . dict-latex-math) (priority . 1))
	    (end "\\\\end{eqnarray}"
		 (dict . dict-latex-math) (priority . 1)))
	   (stack
	    (start "\\\\begin{eqnarray\\*}"
		   (dict . dict-latex-math) (priority . 1))
	    (end "\\\\end{eqnarray\\*}"
		 (dict . dict-latex-math) (priority . 1)))
	
	   ;; \ starts a LaTeX command, which consists either entirely of
	   ;; letter characters, or of a single non-letter character
	   (word ("\\\\\\([[:alpha:]]*?\\)\\([^[:alpha:]]\\|$\\)" . 1)
		 (dict . dict-latex))
	   )))

  
  ;; make "\", "$", "{" and "}" do the right thing
  (setq predictive-override-syntax-alist
	'((?\\ . (lambda () (interactive)
		   (predictive-reject)
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


;;; predictive-latex.el ends here
