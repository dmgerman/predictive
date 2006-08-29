
;;; predictive-latex-smartref.el --- predictive mode LaTeX smartref
;;;                                  package support


;; Copyright (C) 2004-2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
;; Keywords: predictive, latex, package, smartref, sref
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
(provide 'predictive-latex-smartref)

;; add load and unload functions to alist
;;(assoc-delete-all "smartref" predictive-latex-usepackage-functions)
(push '("smartref" predictive-latex-load-smartref
	predictive-latex-unload-smartref)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-smartref ()
  ;; Load sref regexps
  (auto-overlay-load-compound-regexp
   `(start "\\\\sref{" (dict . predictive-latex-label-dict) (priority . 2)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (completion-word-thing . predictive-latex-label-word)
	   (completion-syntax-alist . ((?w . (add t word))
				       (?_ . (add t word))
				       (?  . (accept t none))
				       (?. . (add t word))
				       (t  . (reject t none))))
	   (completion-override-syntax-alist
	    . ((?: . ((lambda ()
			(predictive-latex-completion-add-to-regexp ":"))
		      t word))
	       (?_ . ((lambda ()
			(predictive-latex-completion-add-to-regexp "\\W"))
		      t word))
	       (?} . (accept t none))))
	   (face . (background-color . ,predictive-latex-debug-color)))
   'predictive 'brace t 'sref)
  (auto-overlay-load-compound-regexp
   `(start "\\\\Sref{" (dict . predictive-latex-label-dict) (priority . 2)
	   (completion-menu . predictive-latex-construct-browser-menu)
	   (completion-word-thing . predictive-latex-label-word)
	   (completion-syntax-alist . ((?w . (add t word))
				       (?_ . (add t word))
				       (?  . (accept t none))
				       (?. . (add t word))
				       (t  . (reject t none))))
	   (completion-override-syntax-alist
	    . ((?: . ((lambda ()
			(predictive-latex-completion-add-to-regexp ":"))
		      t word))
	       (?_ . ((lambda ()
			(predictive-latex-completion-add-to-regexp "\\W"))
		      t word))
	       (?} . (accept t none))))
	   (face . (background-color . ,predictive-latex-debug-color)))
   'predictive 'brace t 'Sref)
)



(defun predictive-latex-unload-smartref ()
  ;; Unload sref regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'sref)
  (auto-overlay-unload-regexp 'predictive 'brace 'Sref)
)

;;; predictive-latex-smartref ends here
