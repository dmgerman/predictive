
;;; predictive-latex.el --- predictive mode LaTeX setup function
;;;                         (assumes AMSmath)


;; Copyright (C) 2004-2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.5.4
;; Keywords: predictive, setup function, latex
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Predictive Completion package.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Change Log:
;;
;; Version 0.5.4
;; * updated to reflect changes in completion-ui.el
;;
;; Version 0.5.3
;; * updated to reflect naming changes in dict-tree.el
;;
;; Version 0.5.2
;; * bug fixes
;; * set `predictive-completion-browser-menu' so that LaTeX browser is called
;;   when completing a LaTeX command (can't use overlay-local binding anymore
;;   since LaTeX commands no longer use auto-overlays)
;;
;; Version 0.5.1
;; * renamed to `predictive-latex' and released as only latex setup package
;;
;; Version 0.5
;; * added support for completion browser
;; * modified latex enviroments to use new stack-sync class
;; * stopped using auto-overlays for LaTeX commands, and just set `dict-latex'
;;   as second main dictionary instead
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
;;(require 'auto-overlay-stack-sync)

(provide 'predictive-latex)


;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-latex-regexps nil)
(make-local-variable 'predictive-latex-regexps)

;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-latex-word 'forward-op 'predictive-latex-forward-word)


(defvar predictive-latex-debug-color nil)


(defun predictive-setup-latex ()
  "Sets up predictive mode for use with latex major modes."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-latex)
  (predictive-load-dict 'dict-latex-math)
  (predictive-load-dict 'dict-latex-env)
  (predictive-load-dict 'dict-latex-docclass)
  (predictive-load-dict 'dict-latex-bibstyle)

  ;; add dictionary of latex commands to active dictionaries
  (make-local-variable 'predictive-main-dict)
  (when (atom predictive-main-dict)
    (setq predictive-main-dict (list predictive-main-dict)))
  (add-to-list 'predictive-main-dict 'dict-latex predictive-main-dict)

  ;; use latex browser menu if first character of prefix is "\"
  (make-local-variable 'completion-browser-menu)
  (setq completion-browser-menu
	(lambda (prefix completions)
	  (if (string= (substring prefix 0 1) "\\")
	      (predictive-latex-construct-browser-menu prefix completions)
	    (completion-construct-browser-menu prefix completions))
	  ))
  
  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-latex-regexps)))
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-latex-regexps
	(auto-overlay-init
	 `(
	   ;; %'s start comments that last till end of line
	   (line "%" (dict . predictive-main-dict) (priority . 4)
		 (exclusive . t)
		 (completion-menu
		  . predictive-latex-construct-browser-menu)
		 (face . (background-color . ,predictive-latex-debug-color)))
	   
	   ;; $'s delimit the start and end of inline maths regions
	   (self "\\$" (dict . dict-latex-math) (priority . 3)
		 (completion-menu .
				  predictive-latex-construct-browser-menu)
		 (face . (background-color
			  . ,predictive-latex-debug-color)))
	   
	   ;; \begin{ and \end{ start and end LaTeX environments
	   ;; \text{ starts a text region within a maths display
	   ;; \documentclass starts a document
	   ;; All are ended by } but not by \}. The { is included to ensure
	   ;; all { and } match, but \{ is excluded.
	   (stack
	    (start "\\\\begin{" (dict . dict-latex-env) (priority . 2)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start "\\\\end{" (dict . dict-latex-env) (priority . 2)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start "\\\\text{"
		   (dict . predictive-main-dict)
		   (priority . 2)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start "\\\\documentclass\\(\\[.*\\]\\)?{"
		   (dict . dict-latex-docclass) (priority . 2)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start "\\\\bibliographystyle\\(\\[.*\\]\\)?{"
		   (dict . dict-latex-bibstyle) (priority . 2)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start ("^\\({\\)" . 1) (priority . 2)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start ("[^\\]\\({\\)" . 1) (priority . 2)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (end ("^\\(}\\)" . 1) (priority . 2)
		 (face . (background-color
			  . ,predictive-latex-debug-color)))
	    (end ("[^\\]\\(}\\)" . 1) (priority . 2)
		 (face . (background-color
			  . ,predictive-latex-debug-color))))

	   
	   ;; \begin{...} and \end{...} start and end LaTeX environments
	   (stack
	    (start ("\\\\begin{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
		    0 1)
		   (dict . dict-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (end ("\\\\end{\\(equation\\*?\\|align\\(at\\)?\\*?\\|flalign\\*?\\|gather\\*?\\|multline\\*?\\)}"
		    0 1)
		   (dict . dict-latex-math) (priority . 1)
		   (completion-menu
		    . predictive-latex-construct-browser-menu)
		   (face . (background-color
			    . ,predictive-latex-debug-color)))
	    (start ("\\\\begin{\\(.*?\\)}" 0 1)
		   (priority . 1)
		   (dict . nil)
		   (face . nil))
	    (end ("\\\\end{\\(.*?\\)}" 0 1)
		 (priority . 1)
		 (dict . nil)
		 (face . nil)
		 ))
	   )))
  
  ;; make "\", "$", "{" and "}" do the right thing
  (setq completion-override-syntax-alist
	'((?\\ . ((lambda ()
		    (if (and (char-before) (= (char-before) ?\\))
			'add 'accept))
		  . word))
	  
	  (?{ . ((lambda ()
		   (if (and (char-before) (= (char-before) ?\\))
		    'add 'accept))
		 . (lambda ()
		     (when (auto-overlays-at-point
			    nil '(eq dict dict-latex-env))
		       (complete "") nil))))
	  
	  (?} . (accept . word))
	  (?\( . (accept . word))
	  (?\) . (accept . word))
	  (?$ . (accept . word))
	  (?\" . (accept . (lambda () (TeX-insert-quote nil) nil)))
	  ))

  
  ;; consider \ as start of a word
  (setq completion-word-thing 'predictive-latex-word)
  (set (make-local-variable 'words-include-escapes) nil)
  
  t  ; indicate succesful setup
)



(defun predictive-latex-forward-word (&optional n)
  (let (m)
    ;; going backwards...
    (if (and n (< n 0))
	(unless (bobp)
	  (setq m (- n))
	  (when (= ?\\ (char-before))
	    (while (= ?\\ (char-before)) (backward-char))
	    (setq m (1- m)))
	  (dotimes (i m)
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (while (and (char-before) (= ?\\ (char-before)))
	      (backward-char))))
      ;; going forwards...
      (unless (eobp)
	(setq m (if n n 1))
	(dotimes (i m)
	  (re-search-forward "\\\\\\|\\w" nil t)
	  (backward-char)
	  (re-search-forward "\\\\+\\w*\\|\\w+" nil t)))
      ))
)



(defun predictive-latex-construct-browser-menu (prefix completions)
  "Construct the AMS-LaTeX browser menu keymap."
  
  (completion-construct-browser-menu
   prefix completions 'predictive-latex-browser-menu-item)
)



(defun predictive-latex-browser-menu-item (prefix completion &rest ignore)
  "Construct predictive ams-LaTeX completion browser menu item."
  
  (cond
   ;; if entry is \begin or \end, create sub-menu containing environment
   ;; completions
   ((or (string= (concat prefix completion) "\\begin")
	(string= (concat prefix completion) "\\end"))
    ;; find all latex environments
    (let ((envs (dictree-mapcar (lambda (word entry) word) dict-latex-env))
	  (menu (make-sparse-keymap)))
      (setq envs (mapcar (lambda (e) (concat completion "{" e "}")) envs))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix envs 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
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
	   (dictree-mapcar (lambda (word entry) word) dict-latex-docclass))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix classes 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
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
	   (dictree-mapcar (lambda (word entry) word) dict-latex-bibstyle))
	  (menu (make-sparse-keymap)))
      (setq classes
	    (mapcar (lambda (e) (concat completion "{" e "}")) classes))
      ;; create sub-menu keymap
      (setq menu (completion-browser-sub-menu
		  prefix classes 'predictive-latex-browser-menu-item
		  'completion-browser-sub-menu))
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



;;; predictive-latex.el ends here
