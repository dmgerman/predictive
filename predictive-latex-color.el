
;;; predictive-latex-color.el --- predictive mode LaTeX color package support


;; Copyright (C) 2008, 2013 Toby Cubitt

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
 :syntax-alist ((?w . (predictive-latex-smart-within-braces-resolve-behaviour
		       predictive-latex-word-completion-behaviour)))
 :override-syntax-alist
     ((?} . (predictive-latex-punctuation-resolve-behaviour none)))
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
    (nconc predictive-latex-browser-submenu-alist
	   (list (cons "\\\\\\(text\\|page\\|\\)color" 'dict-latex-colours)))
    ;; add completion source regexps
    (nconc
     auto-completion-source-regexps
     ;; color commands
     (list
      `(,(concat predictive-latex-odd-backslash-regexp
		 "\\(?:\\|text\\|page\\)color\\(?:\\[.*?\\]\\)?"
		 predictive-latex-brace-group-regexp)
	predictive-latex-color looking-at 1))))

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
		   "\\(?:\\|text\\|page\\)color\\(?:\\[.*?\\]\\)?"
		   predictive-latex-brace-group-regexp)
	   auto-completion-source-regexps))
    ;; unload colour dictionary
    (predictive-unload-dict 'dict-latex-colours))
   ))


(provide 'predictive-latex-color)

;;; predictive-latex-color ends here
