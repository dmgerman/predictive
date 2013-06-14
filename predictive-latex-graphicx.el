
;;; predictive-latex-graphicx.el --- predictive mode LaTeX graphicx
;;;                                  package support


;; Copyright (C) 2004-2006, 2008, 2013 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.2
;; Keywords: predictive, latex, package, graphicx
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
(predictive-assoc-delete-all "graphicx" predictive-latex-usepackage-functions)
(push '("graphicx" . predictive-latex-setup-graphicx)
      predictive-latex-usepackage-functions)



(defun predictive-latex-setup-graphicx (arg)
  ;; With positive ARG, load graphicx package support. With negative ARG,
  ;; unload it.
  (cond
   ;; --- load graphicx support ---
   ((> arg 0)
    ;; add completion source regexps
    (make-local-variable 'auto-completion-source-regexps)
    (nconc
     auto-completion-source-regexps
     ;; label with optarg
     `((,(concat predictive-latex-odd-backslash-regexp
		 "includegraphics\\(?:\\[.*?\\]\\)"
		 predictive-latex-brace-group-regexp)
	nil looking-at 1))
     ))

   ;; --- unload graphicx support ---
   ((< arg 0)
    ;; remove completion source regexps
    (setq auto-completion-source-regexps
	  (predictive-assoc-delete-all
	   (concat predictive-latex-odd-backslash-regexp
		   "includegraphics\\(?:\\[.*?\\]\\)"
		   predictive-latex-brace-group-regexp)
	   auto-completion-source-regexps)))
   ))


(provide 'predictive-latex-graphicx)

;;; predictive-latex-graphicx ends here
