
;;; predictive-latex-subfig.el --- predictive mode LaTeX subref
;;;                                package support


;; Copyright (C) 2006-2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
;; Keywords: predictive, latex, package, subref, subfloat, subfig
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
;; * honour user's choices in `auto-completion-syntax-alist' and
;;   `auto-completion-override-syntax-alist'
;; * improved regexp definitions
;;
;; Version 0.2
;; * updated for new auto-overlay regexp definition interface
;;
;; Version 0.1
;; * initial version


;;; Code:

(require 'predictive-latex)
(provide 'predictive-latex-subfig)

;; add load and unload functions to alist
;;(assoc-delete-all "smartref" predictive-latex-usepackage-functions)
(push '("subfig" predictive-latex-load-subfig
	predictive-latex-unload-subfig)
      predictive-latex-usepackage-functions)



(defun predictive-latex-load-subfig ()
  (let* ((word-behaviour (completion-lookup-behaviour nil ?w))
	 (word-complete (completion-get-completion-behaviour word-behaviour))
	 (word-resolve (completion-get-resolve-behaviour word-behaviour))
	 (punct-behaviour (completion-lookup-behaviour nil ?.))
	 (punct-complete (completion-get-completion-behaviour punct-behaviour))
	 (punct-resolve (completion-get-resolve-behaviour punct-behaviour))
	 (whitesp-behaviour (completion-lookup-behaviour nil ? ))
	 (whitesp-complete (completion-get-completion-behaviour
			    whitesp-behaviour))
	 (whitesp-resolve (completion-get-resolve-behaviour
			   whitesp-behaviour)))
    ;; Load subfig regexps
    (auto-overlay-load-regexp
     'predictive 'brace
     `(("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\subref{\\)" . 3)
       :id subref
       :edge start
       (dict . predictive-latex-label-dict)
       (priority . 40)
       (completion-menu . predictive-latex-construct-browser-menu)
       (completion-word-thing . predictive-latex-label-word)
       (completion-dynamic-syntax-alist . ((?w . (add ,word-complete))
					   (?_ . (add ,word-complete))
					   (?  . (,whitesp-resolve none))
					   (?. . (add ,word-complete))
					   (t  . (reject none))))
       (completion-dynamic-override-syntax-alist
	. ((?: . ((lambda ()
		    (predictive-latex-completion-add-to-regexp ":")
		    nil)
		  ,word-complete))
	   (?_ . ((lambda ()
		    (predictive-latex-completion-add-to-regexp "\\W")
		    nil)
		  ,word-complete))
	   (?} . (,punct-resolve t none))))
       (face . (background-color . ,predictive-overlay-debug-color)))
     t)

    )
)



(defun predictive-latex-unload-subfig ()
  ;; Unload subfig regexps
  (auto-overlay-unload-regexp 'predictive 'brace 'subref)
)

;;; predictive-latex-subfig ends here
