
;;; predictive-latex-ntheorem.el --- predictive mode LaTeX ntheorem
;;;                                  package support


;; Copyright (C) 2008, 2012, 2013 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1.2
;; Keywords: predictive, latex, package, ntheorem
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
(predictive-assoc-delete-all "ntheorem" predictive-latex-usepackage-functions)
(push '("ntheorem" . predictive-latex-setup-ntheorem)
      predictive-latex-usepackage-functions)


(defun predictive-latex-setup-ntheorem (arg)
  ;; With positive ARG, load varioref package support. With negative ARG,
  ;; unload it.
  (cond
   ;; --- load ntheorem support ---
   ((> arg 0)
    ;; add completion source regexp
    (make-local-variable 'auto-completion-source-regexps)
    (nconc
     auto-completion-source-regexps
     ;; \thref etc.
     `((,(concat predictive-latex-odd-backslash-regexp
		 "thref" predictive-latex-brace-group-regexp)
	predictive-latex-label looking-at 1)))

    ;; load \newshadedtheorem and \newframedtheorem auto-overlay definitions
    (auto-overlay-load-definition
     'predictive
     `(word
       :id newshadedtheorem
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newshadedtheorem{\\(.*?\\)}" . 3)
    	(auto-dict . predictive-latex-local-env-dict))))
    (auto-overlay-load-definition
     'predictive
     `(word
       :id newframedtheorem
       (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newframedtheorem{\\(.*?\\)}" . 3)
    	(auto-dict . predictive-latex-local-env-dict)))))

   ;; --- unload ntheorem support ---
   ((< arg 0)
    ;; remove completion source regexps
    (setq auto-completion-source-regexps
	  (predictive-assoc-delete-all
	   (concat predictive-latex-odd-backslash-regexp
		   "thref" predictive-latex-brace-group-regexp)
	   auto-completion-source-regexps))

    ;; unload auto-overlay definitions
    (auto-overlay-unload-definition 'predictive 'newshadedtheorem)
    (auto-overlay-unload-definition 'predictive 'newframedtheorem))
   ))


(provide 'predictive-latex-ntheorem)

;;; predictive-latex-ntheorem ends here
