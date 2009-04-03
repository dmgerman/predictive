
;;; predictive-latex-varioref.el --- predictive mode LaTeX varioref
;;;                                  package support


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
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
;; Version 0.1
;; * initial version (adapted from predictive-latex-cleveref.el)


;;; Code:

(require 'predictive-latex)
(provide 'predictive-latex-varioref)

;; add load and unload functions to alist
;;(assoc-delete-all "cleveref" predictive-latex-usepackage-functions)
(push '("varioref" predictive-latex-load-varioref
	predictive-latex-unload-varioref)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-varioref ()
  ;; load cleveref regexps
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w)
	      (auto-completion-lookup-behaviour nil ?.)
	      (auto-completion-lookup-behaviour nil ? ))

    ;; add new browser sub-menu definition
    (make-local-variable 'predictive-latex-browser-submenu-alist)
    (push (cons "\\\\[vV]ref\\(range\\|\\)" 'predictive-latex-label-dict)
	  predictive-latex-browser-submenu-alist)

    ;; load regexps
    ;; \vref
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\[vV]ref\\(\\|range\\)\\*?{\\)" . 3)
       :edge start
       :id vref
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (auto-completion-syntax-alist . ((?w . (add ,word-complete))
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
     t)))



(defun predictive-latex-unload-varioref ()
  ;; remove browser sub-menu definition
  (setq predictive-latex-browser-submenu-alist
	(predictive-assoc-delete-all "\\\\[vV]ref\\(range\\|\\)"
				     predictive-latex-browser-submenu-alist))
  ;; Unload cleveref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'cref))


;;; predictive-latex-varioref ends here
