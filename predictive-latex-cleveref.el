
;;; predictive-latex-cleveref.el --- predictive mode LaTeX cleveref
;;;                                  package support


;; Copyright (C) 2004-2013 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.10
;; Keywords: predictive, latex, package, cleveref, cref
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is NOT part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'predictive-latex)

;; register package setup function
(predictive-assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("cleveref" . predictive-latex-setup-cleveref)
      predictive-latex-usepackage-functions)

;; set up `predictive-latex-cleveref-label-word' to be a `thing-at-point'
;; symbol
(put 'predictive-latex-cleveref-label-word 'forward-op
     'predictive-latex-cleveref-label-forward-word)

;; variable used to hold old definition of label auto-overlay
(defvar predictive-latex-cleveref-restore-label-definition nil)




;;;=========================================================
;;;     Cleveref `auto-completion-at-point-functions'

;; derive cleveref-style label completion source from standard label source
(define-completion-at-point-function
  predictive-latex-cleveref-label-completion-at-point
  predictive-complete
  :name predictive-latex-label   ; inherit customization
  :type label
  :completion-args 2
  :other-args (predictive-latex-label-dict)
  :word-thing 'predictive-latex-cleveref-label-word
  :allow-empty-prefix t
  :activate-function (lambda ()
		       (looking-back
			(concat predictive-latex-odd-backslash-regexp
			 "\\(?:[cCvV]ref\\(?:\\|range\\)\\*?"
			 "\\|\\(?:name\\|label\\)[cC]ref\\)"
			 predictive-latex-brace-group-regexp)
			(line-beginning-position)))
  :syntax-alist
   ((?w . (add predictive-latex-word-completion-behaviour
	       predictive-latex-cleveref-smart-wthin-braces-insert-behaviour))
    (?_ . (add predictive-latex-word-completion-behaviour
	       predictive-latex-cleveref-smart-wthin-braces-insert-behaviour))
    (?. . (add predictive-latex-word-completion-behaviour
	       predictive-latex-cleveref-smart-wthin-braces-insert-behaviour))
    (?  . (predictive-latex-whitespace-resolve-behaviour none))
    (t  . (reject none)))
  :override-syntax-alist
   ((?: . ((lambda ()
	     (predictive-latex-completion-add-till-regexp ":"))
	   predictive-latex-word-completion-behaviour))
    (?_ . ((lambda ()
	     (predictive-latex-completion-add-till-regexp "\\W"))
	   predictive-latex-word-completion-behaviour))
    (?, . (predictive-latex-punctuation-resolve-behaviour none))
    (?} . (predictive-latex-punctuation-resolve-behaviour none)))
  :no-auto-completion t
  :no-predictive t
  :no-command t)


(defun predictive-latex-cleveref-label-no-completion-at-point ()
  "Function used in `completion-at-point-functions'
to disable completion within a \"\\label\" argument."
  (when (looking-back
	 (concat predictive-latex-odd-backslash-regexp
		 "label\\(?:\\[.*?\\]\\)?"
		 predictive-latex-brace-group-regexp)
	 (line-beginning-position))
    t))




;;;============================================================
;;;                       Setup function

(defun predictive-latex-setup-cleveref (&optional arg)
  ;; With positive ARG, load cleveref package support. With negative ARG,
  ;; unload it.
  (cond

   ;; --- load cleveref support ---
   ((> arg 0)
    ;; add new browser sub-menu definition
    (nconc predictive-latex-browser-submenu-alist
	   (list (cons "\\\\[cC]ref\\(range\\|\\)"
		       'predictive-latex-label-dict)))

    ;; add new `auto-completion-at-point-functions' function
    (setq auto-completion-at-point-functions
	  (predictive-latex-insert-after
	   auto-completion-at-point-functions
	   'predictive-latex-label-completion-at-point
	   'predictive-latex-cleveref-label-completion-at-point))

    ;; replace label completion-disabling `auto-completion-at-point-functions'
    ;; function
    ;; FIXME: assumes 'predictive-latex-label-no-completion is never first
    ;; element of `auto-completion-at-point-functions'
    (setcar (memq 'predictive-latex-label-no-completion-at-point
		  auto-completion-at-point-functions)
	    'predictive-latex-cleveref-label-no-completion-at-point)

    ;; cleveref auto-overlay definition replaces standard label definition
    (setq predictive-latex-cleveref-restore-label-definition
	  (auto-overlay-unload-definition 'predictive 'label))
    (auto-overlay-load-definition
     'predictive
     '(predictive-auto-dict
       :id label
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label\\(\\[.*?\\]\\)?{\\(.*?\\)}"
	 . 4)
	(auto-dict . predictive-latex-label-dict)))
     t))


   ;; --- unload cleveref support ---
   ((< arg 0)
    ;; remove browser sub-menu definition
    (setq predictive-latex-browser-submenu-alist
	  (predictive-assoc-delete-all
	   "\\\\[cC]ref\\(range\\|\\)"
	   predictive-latex-browser-submenu-alist))

    ;; unload cleveref auto-overlay definition
    (auto-overlay-load-definition
     'predictive predictive-latex-cleveref-restore-label-definition)
    (kill-local-variable 'predictive-latex-cleveref-restore-label-definition)

    ;; restore label completion-disabling `auto-completion-at-point-functions'
    ;; function
    (setcar (memq 'predictive-latex-label-no-completion
		  auto-completion-at-point-functions)
	    'predictive-latex-cleveref-label-no-completion)

    ;; remove `auto-completion-at-point-functions' function
    (setq auto-completion-at-point-functions
	  (delq 'predictive-latex-cleveref-label-completion-at-point
		auto-completion-at-point-functions)))
   ))




;;;=============================================================
;;;               Miscelaneous utility functions

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
    ))


(defun predictive-latex-cleveref-smart-within-braces-insert-behaviour ()
  (predictive-latex-smart-within-braces-insert-behaviour "\\([,}]\\)"))



(provide 'predictive-latex-cleveref)

;;; predictive-latex-cleveref ends here
