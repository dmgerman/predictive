
;;; predictive-latex-cleveref.el --- predictive mode LaTeX cleveref
;;;                                  package support


;; Copyright (C) 2004-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.6.1
;; Keywords: predictive, latex, package, cleveref, cref
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
;; Version 0.6.1
;; * changed predictive-latex-label overlay class in label overlays to the new
;;   predictive-latex-auto-dict class
;;
;; Version 0.6
;; * allow \label to take optional argument
;; * switched ordering of `auto-completion[-override]-syntax-alist' entries to
;;   conform to new completion-ui
;; * bug-fixes
;;
;; Version 0.5
;; * updated for new auto-overlay regexp definition interface
;;
;; Version 0.4
;; * renamed to "cleveref" to match package name change
;;
;; Version 0.3
;; * updated for new version of smartref package
;;
;; Version 0.2.1
;; * updated `completion-override-syntax-alist' settings to reflect changes in
;;   predictive-latex.el
;; 
;; Version 0.2
;; * added overlay-local `completion-override-syntax-alist' bindings
;;
;; Version 0.1
;; * initial version


;;; Code:

(require 'predictive-latex)
(provide 'predictive-latex-cleveref)

;; add load and unload functions to alist
;;(assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("cleveref" predictive-latex-load-cleveref
	predictive-latex-unload-cleveref)
      predictive-latex-usepackage-functions)


;; set up 'predictive-latex-cleveref-label-word to be a `thing-at-point'
;; symbol
(put 'predictive-latex-cleveref-label-word 'forward-op
     'predictive-latex-cleveref-label-forward-word)


;; variables used to hold old definitions of label regexps
(defvar predictive-latex-cleveref-restore-label-regexp nil)
(defvar predictive-latex-cleveref-restore-label-definition nil)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-regexp)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-definition)



(defun predictive-latex-load-cleveref ()
  ;; load cleveref regexps
  
  ;; \cref
  (auto-overlay-load-regexp
   'predictive 'brace
   `("\\\\cref{"
     :edge start
     :id cref
     (dict . predictive-latex-label-dict)
     (priority . 40)
     (completion-menu . predictive-latex-construct-browser-menu)
     (completion-word-thing . predictive-latex-cleveref-label-word)
     (auto-completion-syntax-alist . ((?w . (add word))
				      (?_ . (add word))
				      (?  . (accept none))
				      (?. . (add word))
				      (t  . (reject none))))
     (auto-completion-override-syntax-alist
      . ((?: . ((lambda ()
		  (predictive-latex-completion-add-till-regexp ":")
		  nil)
		word))
	 (?_ . ((lambda ()
		  (predictive-latex-completion-add-till-regexp "\\W")
		  nil)
		word))
	 (?, . (accept none))
	 (?} . (accept none))))
     (face . (background-color . ,predictive-overlay-debug-color)))
   t)
  
  ;; \Cref
  (auto-overlay-load-regexp
   'predictive 'brace
   `("\\\\Cref{"
     :edge start
     :id Cref
     (dict . predictive-latex-label-dict)
     (priority . 40)
     (completion-menu . predictive-latex-construct-browser-menu)
     (completion-word-thing . predictive-latex-cleveref-label-word)
     (auto-completion-syntax-alist . ((?w . (add word))
				      (?_ . (add word))
				      (?  . (accept none))
				      (?. . (add word))
				      (t  . (reject none))))
     (auto-completion-override-syntax-alist
      . ((?: . ((lambda ()
		  (predictive-latex-completion-add-till-regexp ":")
		  nil)
		word))
	 (?_ . ((lambda ()
		  (predictive-latex-completion-add-till-regexp "\\W")
		  nil)
		word))
	 (?, . (accept none))
	 (?} . (accept none))))
     (face . (background-color . ,predictive-overlay-debug-color)))
   t)

  ;; \crefrange
  (auto-overlay-load-regexp
   'predictive 'brace
   `("\\\\crefrange{"
     :edge start
     :id crefrange
     (dict . predictive-latex-label-dict)
     (priority . 40)
     (completion-menu . predictive-latex-construct-browser-menu)
     (completion-word-thing . predictive-latex-cleveref-label-word)
     (auto-completion-syntax-alist . ((?w . (add word))
				      (?_ . (add word))
				      (?  . (accept none))
				      (?. . (add word))
				      (t  . (reject none))))
     (auto-completion-override-syntax-alist
      . ((?: . ((lambda ()
		  (predictive-latex-completion-add-till-regexp ":")
		  nil)
		word))
	 (?_ . ((lambda ()
		  (predictive-latex-completion-add-till-regexp "\\W")
		  nil)
		word))
	 (?, . (accept none))
	 (?} . (accept none))))
     (face . (background-color . ,predictive-overlay-debug-color)))
   t)

  ;; \Crefrange
  (auto-overlay-load-regexp
   'predictive 'brace
   `("\\\\Crefrange{"
     :edge start
     :id Crefrange
     (dict . predictive-latex-label-dict)
     (priority . 40)
     (completion-menu . predictive-latex-construct-browser-menu)
     (completion-word-thing . predictive-latex-cleveref-label-word)
     (auto-completion-syntax-alist . ((?w . (add word))
				      (?_ . (add word))
				      (?  . (accept none))
				      (?. . (add word))
				      (t  . (reject none))))
     (auto-completion-override-syntax-alist
      . ((?: . ((lambda ()
		  (predictive-latex-completion-add-till-regexp ":")
		  nil)
		word))
	 (?_ . ((lambda ()
		  (predictive-latex-completion-add-till-regexp "\\W")
		  nil)
		word))
	 (?, . (accept none))
	 (?} . (accept none))))
     (face . (background-color . ,predictive-overlay-debug-color)))
   t)


  ;; \label with optional argument
  (setq predictive-latex-cleveref-restore-label-regexp
	(auto-overlay-unload-regexp 'predictive 'brace 'label))
  (auto-overlay-load-regexp
   'predictive 'brace
   `("\\\\label\\(\\[.*?\\]\\)?{"
     :edge start
     :id label
     (dict . t)
     (priority . 40)
     (face . (background-color . ,predictive-overlay-debug-color)))
   t)
  
  (setq predictive-latex-cleveref-restore-label-definition
	(auto-overlay-unload-definition 'predictive 'label))
  (auto-overlay-load-definition
   'predictive
   '(predictive-latex-auto-dict
     :id label
     (("\\\\label\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 2)
      (auto-dict . predictive-latex-label-dict))))
)



(defun predictive-latex-unload-cleveref ()
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'cref)
  (auto-overlay-unload-regexp 'predictive 'brace 'Cref)
  (auto-overlay-unload-regexp 'predictive 'brace 'crefrange)
  (auto-overlay-unload-regexp 'predictive 'brace 'Crefrange)
  (auto-overlay-unload-regexp 'predictive 'brace 'label)
  (auto-overlay-load-regexp
   'predictive 'brace predictive-latex-cleveref-restore-label-regexp t)
  (auto-overlay-unload-definition 'predictive 'label)
  (auto-overlay-load-definition
   'predictive predictive-latex-cleveref-restore-label-definition)
)



(defun predictive-latex-cleveref-label-forward-word (&optional n)
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
	    (while (and (char-before)
			(or (= (char-syntax (char-before)) ?w)
			    (= (char-syntax (char-before)) ?_)
			    (and (= (char-syntax (char-before)) ?.)
				 (/= (char-before) ?,))))
	      (backward-char))))
      ;; going forwards...
      (unless (eobp)
	(setq m (if n n 1))
	(dotimes (i m)
	  (if (re-search-forward "\\(\\w\\|\\s_\\|\\s.\\)+" nil t)
	      (when (= (char-before) ?,) (backward-char))
	    (goto-char (point-max)))))
      ))
)

;;; predictive-latex-cleveref ends here
