
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
(define-completion-at-point-function
  predictive-latex-color-completion-at-point
  predictive-complete
  :name predictive-latex-color    ; use different name here to avoid
  :type color                     ; predictive-latex customizations
  :completion-args 2              ; overriding :syntax-alist
  :other-args (dict-latex-colours)
  :word-thing 'predictive-latex-word
  :allow-empty-prefix t
  :activate-function (lambda ()
		       (looking-back
			(concat predictive-latex-odd-backslash-regexp
				"\\(?:\\|text\\|page\\)color\\(?:\\[.*?\\]\\)?"
				predictive-latex-brace-group-regexp)
			(line-beginning-position)))
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

    ;; add new `auto-completion-at-point-functions' function
    (setq auto-completion-at-point-functions
	  (predictive-latex-insert-after
	   auto-completion-at-point-functions
	   'predictive-latex-text-completion-at-point
	   'predictive-latex-color-completion-at-point)))

   ;; --- unload color support ---
   ((< arg 0)
    ;; remove browser sub-menu definition
    (setq predictive-latex-browser-submenu-alist
	  (predictive-assoc-delete-all
	   "\\\\\\(text\\|page\\|\\)color"
	   predictive-latex-browser-submenu-alist))

    ;; unload colour dictionary
    (predictive-unload-dict 'dict-latex-colours)

    ;; remove `auto-completion-at-point-functions' function
    (setq auto-completion-at-point-functions
	  (delq 'predictive-latex-color-completion-at-point
		auto-completion-at-point-functions)))
   ))


;; FIXME: We could define a color predictive-auto-dict overlay so new colour
;;        definitions are automatically added to a local color
;;        dictionary. (Could even extend `predictive-latex-jump-to-definition'
;;        to work for colors, though this would take more coding.) Might not
;;        be worth it just for colours, though.


(provide 'predictive-latex-color)

;;; predictive-latex-color ends here
