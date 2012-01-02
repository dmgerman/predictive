
;;; predictive-latex-cleveref.el --- predictive mode LaTeX cleveref
;;;                                  package support


;; Copyright (C) 2004-2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.9.1
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
;; Version 0.9.1
;; * fixed deletion behaviour inside \cref{...} etc.
;;
;; Version 0.9
;; * updated to include \namecref and \labelcref from latest cleveref package
;;
;; Version 0.8.1
;; * modified `auto-completion-syntax-alist' for "\cref{" etc. so that typing
;;   a word-constituent deletes everything from point up to the end of the
;;   argument.
;;
;; Version 0.8
;; * cooperate with predictive-latex-varioref.el
;;
;; Version 0.7
;; * added completion browser sub-menu definition
;; * improved regexp definitions
;;
;; Version 0.6.5
;; * honour user's choices in `auto-completion-syntax-alist' and
;;   `auto-completion-override-syntax-alist'
;; * improved regexp definitions
;;
;; Version 0.6.4
;; * updated to reflect renaming of predictive-auto-dict overlay class
;;
;; Version 0.6.3
;; * improved `predictive-latex-cleveref-label-forward-word' (again!)
;;
;; Version 0.6.2
;; * fixed bug in `predictive-latex-cleveref-label-forward-word'
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
(defvar predictive-latex-cleveref-restore-vref-regexp nil)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-regexp)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-label-definition)
(make-variable-buffer-local
 'predictive-latex-cleveref-restore-vref-definition)



(defun predictive-latex-load-cleveref ()
  ;; load cleveref regexps
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))

    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (push (cons "\\\\[cC]ref\\(range\\|\\)" 'predictive-latex-label-dict)
	  predictive-latex-browser-submenu-alist)

    ;; load regexps
    ;; \cref and \vref
    (setq predictive-latex-cleveref-restore-vref-regexp
	  (auto-overlay-unload-regexp 'predictive 'brace 'vref))
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\\\([cCvV]ref\\(\\|range\\)\\*?\\|\\(name\\|label\\)[cC]ref\\){\\)"
	. 3)
       :edge start
       :id cref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (completion-word-thing . predictive-latex-cleveref-label-word)
       (auto-completion-syntax-alist
	. ((?w . ((lambda ()
		    (let ((label (bounds-of-thing-at-point
				  'predictive-latex-cleveref-label-word)))
		      (when (and label (= (point) (car label)))
			(delete-region (car label) (cdr label))))
		    'add)
		  ,word-complete t))
	   (?_ . (add ,word-complete))
	   (?  . (,whitesp-resolve none))
	   (?. . (add ,word-complete))
	   (t  . (reject none))))
       (auto-completion-override-syntax-alist
	. ((?: . ((lambda ()
		    (predictive-latex-completion-add-till-regexp ":"))
		  ,word-complete))
	   (?_ . ((lambda ()
		    (predictive-latex-completion-add-till-regexp "\\W"))
		  ,word-complete))
	   (?, . (,punct-resolve none))
	   (?} . (,punct-resolve none))))
       (face . (background-color . ,predictive-overlay-debug-color)))
     t)

    ;; \label with optional argument replaces normal \label regexps
    (setq predictive-latex-cleveref-restore-label-regexp
	  (auto-overlay-unload-regexp 'predictive 'brace 'label))
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\label\\(\\[.*?\\]\\)?{\\)" . 3)
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
     '(predictive-auto-dict
       :id label
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label\\(\\[.*?\\]\\)?{\\(.*?\\)}"
	 . 4)
       (auto-dict . predictive-latex-label-dict)))
     t)))



(defun predictive-latex-unload-cleveref ()
  ;; remove browser sub-menu definition
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\[cC]ref\\(range\\|\\)"
				     predictive-latex-browser-submenu-alist))
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'cref)
  (when predictive-latex-cleveref-restore-vref-regexp
    (auto-overlay-load-regexp
     'predictive 'brace predictive-latex-cleveref-restore-vref-regexp t))
  (auto-overlay-unload-regexp 'predictive 'brace 'label)
  (auto-overlay-unload-definition 'predictive 'label)
  (auto-overlay-load-regexp
   'predictive 'brace predictive-latex-cleveref-restore-label-regexp t)
  (auto-overlay-load-definition
   'predictive predictive-latex-cleveref-restore-label-definition)
  (kill-local-variable 'predictive-latex-cleveref-restore-vref-regexp)
  (kill-local-variable 'predictive-latex-cleveref-restore-label-regexp)
  (kill-local-variable 'predictive-latex-cleveref-restore-label-definition))



(defun predictive-latex-cleveref-label-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(when (= ?\\ (char-before))
	  (while (= ?\\ (char-before)) (backward-char))
	  (setq n (1- n)))
	(dotimes (i n)
	  (when (and (char-before) (= (char-syntax (char-before)) ?w))
	    (backward-word 1))  ; argument not optional in Emacs 21
	  (while (and (char-before)
		      (or (= (char-syntax (char-before)) ?w)
			  (= (char-syntax (char-before)) ?_)
			  (and (= (char-syntax (char-before)) ?.)
			       (/= (char-before) ?,)
			       (/= (char-before) ?{))))
	    (backward-char))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(when (and (char-after) (= (char-syntax (char-after)) ?w))
	  (forward-word 1))  ; argument not optional in Emacs 21
	(while (and (char-after)
		    (or (= (char-syntax (char-after)) ?w)
			(= (char-syntax (char-after)) ?_)
			(and (= (char-syntax (char-after)) ?.)
			     (/= (char-after) ?,)
			     (/= (char-after) ?}))))
	  (forward-char))))
;;; 	(if (re-search-forward "\\(\\w\\|\\s_\\|\\s.\\)+" nil t)
;;; 	    (when (= (char-before) ?,) (backward-char))
;;; 	  (goto-char (point-max)))))
    )
)

;;; predictive-latex-cleveref ends here
