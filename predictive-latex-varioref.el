
;;; predictive-latex-varioref.el --- predictive mode LaTeX varioref
;;;                                  package support


;; Copyright (C) 2009, 2012-2013 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.3
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
(predictive-assoc-delete-all "varioref" predictive-latex-usepackage-functions)
(push '("varioref" . predictive-latex-setup-varioref)
      predictive-latex-usepackage-functions)



(defun predictive-latex-setup-varioref (arg)
  ;; With positive ARG, load varioref package support. With negative ARG,
  ;; unload it.
  (cond
   ;; --- load varioref support ---
   ((> arg 0)
    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (nconc predictive-latex-browser-submenu-alist
	   '(("\\\\[vV]\\(\\|page\\)ref\\(range\\|\\)"
	      . predictive-latex-label-dict)
	     ("\\\\fullref" . predictive-latex-label-dict)))

    ;; add completion source regexps
    (make-local-variable 'auto-completion-source-regexps)
    (nconc
     auto-completion-source-regexps
     ;; \vref etc.
     `((,(concat predictive-latex-odd-backslash-regexp
		 "[vV]\\(?:\\|page\\)ref\\(?:\\|range\\)\\*?"
		 predictive-latex-brace-group-regexp)
	predictive-latex-label looking-at 1)
       ;; \fullref
       (,(concat predictive-latex-odd-backslash-regexp
		 "fullref" predictive-latex-brace-group-regexp)
	predictive-latex-label looking-at 1))))

   ;; --- unload varioref support ---
   ((< arg 0)
    ;; remove browser sub-menu definition
    (setq predictive-latex-browser-submenu-alist
	  (predictive-assoc-delete-all
	   "\\\\[vV]\\(\\|page\\)ref\\(range\\|\\)"
	   predictive-latex-browser-submenu-alist))
    (setq predictive-latex-browser-submenu-alist
	  (predictive-assoc-delete-all
	   "\\\\fullref" predictive-latex-browser-submenu-alist))

    ;; remove completion source regexps
    (setq auto-completion-source-regexps
	  (predictive-assoc-delete-all
	   (concat predictive-latex-odd-backslash-regexp
		      "[vV]\\(?:\\|page\\)ref\\(?:\\|range\\)\\*?"
		      predictive-latex-brace-group-regexp)
	   auto-completion-source-regexps))
    (setq auto-completion-source-regexps
	  (predictive-assoc-delete-all
	   (concat predictive-latex-odd-backslash-regexp
		      "fullref" predictive-latex-brace-group-regexp)
	   auto-completion-source-regexps))
    )))


(provide 'predictive-latex-varioref)

;;; predictive-latex-varioref ends here
