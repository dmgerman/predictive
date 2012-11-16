
;;; predictive-latex-color.el --- predictive mode LaTeX color package support


;; Copyright (C) 2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
;; Keywords: predictive, latex, package, color
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
(predictive-assoc-delete-all "color" predictive-latex-usepackage-functions)
(push '("color" . predictive-latex-setup-color)
      predictive-latex-usepackage-functions)


;; define LaTeX colour completion source
(completion-ui-register-source
 predictive-complete
 :name predictive-latex-color
 :completion-args 2
 :other-args (dict-latex-colours)
 :accept-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-accept-functions
					 prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-reject-functions
					 prefix completion arg))
 :syntax-alist ((?w . ((lambda ()
			 (let ((env (bounds-of-thing-at-point
				     'predictive-latex-word)))
			   (when (and env (= (point) (car env)))
			     (delete-region (car env) (cdr env))))
			 'add)
		       predictive-latex-word-completion-behaviour t)))
 :override-syntax-alist
     ((?} predictive-latex-punctuation-resolve-behaviour 'none))
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-function
 :no-auto-completion t
 :no-command t
 :no-predictive t)



(defun predictive-latex-setup-color (arg)
  ;; With positive ARG, load cleveref package support. With negative ARG,
  ;; unload it.
  (cond
   ;; --- load color support ---
   ((> arg 0)
    ;; load colour dictionary
    (predictive-load-dict 'dict-latex-colours)
    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (push (cons "\\\\\\(text\\|page\\|\\)color" 'dict-latex-colours)
	  predictive-latex-browser-submenu-alist)
    ;; add completion source regexps
    (set (make-local-variable 'auto-completion-source-regexps)
	 (nconc
	  ;; label with optarg
	  `((,(concat predictive-latex-odd-backslash-regexp
		      "\\(\\|text\\|page\\)color\\(\\[.*?\\]\\)?{"
    		      predictive-latex-not-closebrace-regexp)
	     looking-at predictive-latex-color))
	  auto-completion-source-regexps)))

   ;; --- unload color support ---
   ((< arg 0)
    ;; remove browser sub-menu definition
    (setq predictive-latex-browser-submenu-alist
	  (predictive-assoc-delete-all
	   "\\\\\\(text\\|page\\|\\)color"
	   predictive-latex-browser-submenu-alist))
    ;; remove auto-completion source regexps
    (setq auto-completion-source-regexps
	  (predictive-assoc-delete-all
	   (concat predictive-latex-odd-backslash-regexp
		   "\\(\\|text\\|page\\)color\\(\\[.*?\\]\\)?{\\)"
		   predictive-latex-not-closebrace-regexp)
	   auto-completion-source-regexps))
    ;; unload colour dictionary
    (predictive-unload-dict 'dict-latex-colours))
   ))


(provide 'predictive-latex-color)

;;; predictive-latex-color ends here
