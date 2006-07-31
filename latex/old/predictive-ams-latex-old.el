
;;; predictive-setup-latex-ams.el --- predictive mode LaTeX setup function
;;                                    (for AMSmath users)


;; Copyright (C) 2004 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.5
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
;; Version 0.5
;; * added support for completion browser
;; * modified latex enviroments to use new stack-sync class
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
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-stack)
(require 'auto-overlay-stack-sync)

(provide 'predictive-ams-latex)


;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-ams-latex-regexps nil)
(make-local-variable 'predictive-ams-latex-regexps)

;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-latex-word 'forward-op 'predictive-latex-forward-word)




(defun predictive-setup-ams-latex ()
  "Sets up predictive mode for use with latex major modes."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-ams-latex)
  (predictive-load-dict 'dict-ams-latex-math)
  (predictive-load-dict 'dict-ams-latex-env)
  (predictive-load-dict 'dict-latex-docclass)
  (predictive-load-dict 'dict-latex-bibstyle)
  
  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-ams-latex-regexps)))
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-ams-latex-regexps
	(auto-overlay-init
	 `(
	   ;; %'s start comments that last till end of line
	   (line "%" (dict . predictive-main-dict) (priority . 4)
		 (exclusive . t)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu))
	   
	   ;; $'s delimit the start and end of inline maths regions
	   (self "\\$" (dict . dict-ams-latex-math) (priority . 3)
		 (completion-menu .
				  predictive-ams-latex-generate-browser-menu))
	   
	   ;; \begin{ and \end{ start and end LaTeX environments
	   ;; \text{ starts a text region within a maths display
	   ;; \documentclass starts a document
	   ;; All are ended by } but not by \}. The { is included to ensure
	   ;; all { and } match, but \{ is excluded.
	   (stack
	    (start "\\\\begin{" (dict . dict-ams-latex-env) (priority . 2)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (start "\\\\end{" (dict . dict-ams-latex-env) (priority . 2)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (start "\\\\text{"
		   (dict . (list predictive-main-dict predictive-buffer-dict))
		   (priority . 2)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (start "\\\\documentclass\\(\\[.*\\]\\)?{"
		   (dict . dict-latex-docclass) (priority . 2)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (start "\\\\bibliographystyle\\(\\[.*\\]\\)?{"
		   (dict . dict-latex-bibstyle) (priority . 2)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (start ("^\\({\\)" . 1) (priority . 2))
	    (start ("[^\\]\\({\\)" . 1) (priority . 2))
	    (end ("^\\(}\\)" . 1) (priority . 2))
	    (end ("[^\\]\\(}\\)" . 1) (priority . 2)))

	   
	   ;; \begin{...} and \end{...} start and end various maths displays
	   (stack
	    (start "\\\\begin{equation}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{equation}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{equation\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{equation\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{align}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{align}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{align\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{align\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{alignat}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{alignat}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{alignat\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{alignat\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{flalign}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{flalign}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{flalign\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{flalign\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{gather}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{gather}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{gather\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{gather\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{multline}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{multline}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   (stack
	    (start "\\\\begin{multline\\*}"
		   (dict . dict-ams-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-ams-latex-generate-browser-menu))
	    (end "\\\\end{multline\\*}"
		 (dict . dict-ams-latex-math) (priority . 1)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu)))
	   
	   ;; \ starts a LaTeX command, which consists either entirely of
	   ;; letter characters, or of a single non-letter character
	   (word ("\\\\\\([[:alpha:]]*?\\)\\([^[:alpha:]]\\|$\\)" . 1)
		 (dict . dict-ams-latex)
		 (completion-menu
		  . predictive-ams-latex-generate-browser-menu))
	   )))
  
  
  ;; word-constituents add to the current completion, symbol-constituents
  ;; reject it, punctuation and whitespace accept it, anything else rejects
  (setq predictive-syntax-alist
	'((?w . predictive-insert-and-complete-word-at-point)
	  (?_ . predictive-reject-and-insert)
	  (?  . predictive-accept-and-insert)
	  (?. . predictive-accept-and-insert)
	  (t  . predictive-reject-and-insert)))
  
  ;; make "\", "$", "{" and "}" do the right thing
  (setq predictive-override-syntax-alist
	'((?\\ . (lambda ()
		 (unless (and (char-before) (= (char-before) ?\\))
		   (completion-accept))
		 (predictive-insert-and-complete)))
	  (?{ . (lambda ()
		(if (and (char-before) (= (char-before) ?\\))
		    (predictive-insert-and-complete)
		  (predictive-accept-and-insert)
		  (when (auto-overlays-at-point
			 nil '(eq dict dict-ams-latex-env))
		    (predictive-complete "")))))
	  (?} . predictive-accept-and-insert)
	  (?\( . predictive-accept-and-insert)
	  (?\) . predictive-accept-and-insert)
	  (?$ . predictive-accept-and-insert)
	  (?\" . (lambda () (completion-accept) (TeX-insert-quote nil)))))

  ;; consider \ as start of a word
  (setq predictive-word-thing 'predictive-latex-word)
  (set (make-local-variable 'words-include-escapes) nil)
  
  t  ; indicate succesful setup
)



(defun predictive-latex-forward-word (&optional n)
  (let (m)
    ;; going backwards...
    (if (and n (< n 0))
	(progn
	  (setq m (- n))
	  (when (= ?\\ (char-before))
	    (while (= ?\\ (char-before)) (backward-char))
	    (setq m (1- m)))
	  (dotimes (i m)
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (while (and (char-before) (= ?\\ (char-before)))
	      (backward-char))))
      ;; going forwards...
      (setq m (if n n 1))
      (dotimes (i m)
	(re-search-forward "\\\\\\|\\w" nil t)
	(backward-char)
	(re-search-forward "\\\\+\\w*\\|\\w+" nil t))
      ))
)



(defun predictive-ams-latex-generate-browser-menu (prefix completions)
  "Construct the AMS-LaTeX browser menu keymap."
  
  (predictive-completion-generate-browser-menu
   prefix completions 'predictive-ams-latex-browser-menu-item)
)



(defun predictive-ams-latex-browser-menu-item (prefix completion &rest ignore)
  "Construct predictive ams-LaTeX completion browser menu item."
  
  (cond
   ;; if entry is \begin or \end, create sub-menu containing environment
   ;; completions
   ((or (string= (concat prefix completion) "\\begin")
	(string= (concat prefix completion) "\\end"))
    ;; find all latex environments
    (let ((envs (dict-mapcar (lambda (word entry) word) dict-ams-latex-env))
	  (menu (make-sparse-keymap)))
      (setq envs (mapcar (lambda (e) (concat completion "{" e "}")) envs))
      ;; create sub-menu keymap
      (setq menu (predictive-completion-browser-sub-menu
		  prefix envs 'predictive-ams-latex-browser-menu-item
		  'predictive-completion-browser-sub-menu))
      ;; add completion itself (\begin or \end) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; if entry is \documentclass, create sub-menu containing environment
   ;; completions
   ((string= (concat prefix completion) "\\documentclass")
    ;; find all latex docclasses
    (let ((classes
	   (dict-mapcar (lambda (word entry) word) dict-latex-docclass))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (predictive-completion-browser-sub-menu
		  prefix classes 'predictive-ams-latex-browser-menu-item
		  'predictive-completion-browser-sub-menu))
      ;; add completion itself (i.e. \documentclass) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; if entry is \bibliographystyle, create sub-menu containing bib styles
   ((string= (concat prefix completion) "\\bibliographystyle")
    ;; find all bib styles
    (let ((classes
	   (dict-mapcar (lambda (word entry) word) dict-latex-bibstyle))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (predictive-completion-browser-sub-menu
		  prefix classes 'predictive-ams-latex-browser-menu-item
		  'predictive-completion-browser-sub-menu))
      ;; add completion itself (i.e. \bibliographystyle) to the menu
      (define-key menu [separator-item-sub-menu] '(menu-item "--"))
      (define-key menu [completion-insert-root]
	(list 'menu-item (concat prefix completion)
	      `(lambda () (insert ,completion))))
      ;; return the menu keymap
      menu))
   
   
   ;; otherwise, create a selectable completion item
   (t `(lambda () (insert ,completion))))
)



;;; predictive-ams-latex.el ends here
