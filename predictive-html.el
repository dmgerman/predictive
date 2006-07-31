
;;; predictive-setup-html.el --- predictive mode HTML setup function


;; Copyright (C) 2005 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: predictive, setup function, html
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
;; * updated to reflect changes in completion-ui.el
;;
;; Version 0.2
;; * modified to use the new auto-overlays package
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(provide 'predictive-html)

;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-html-regexps nil)
(make-local-variable 'predictive-html-regexps)


(defvar predictive-html-debug-color nil)


(defun predictive-setup-html ()
  "Sets up predictive mode for use with html major modes."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-html)
  ;; general attributes
  (predictive-load-dict 'dict-html-common)
  (predictive-load-dict 'dict-html-core)
  (predictive-load-dict 'dict-html-events)
  (predictive-load-dict 'dict-html-international)
  ;; tag-specific attributes
  (predictive-load-dict 'dict-html-a)
  (predictive-load-dict 'dict-html-area)
  (predictive-load-dict 'dict-html-base)
  (predictive-load-dict 'dict-html-quote)
  (predictive-load-dict 'dict-html-body)
  (predictive-load-dict 'dict-html-button)
  (predictive-load-dict 'dict-html-col)
  (predictive-load-dict 'dict-html-del)
  (predictive-load-dict 'dict-html-form)
  (predictive-load-dict 'dict-html-head)
  (predictive-load-dict 'dict-html-img)
  (predictive-load-dict 'dict-html-input)
  (predictive-load-dict 'dict-html-ins)
  (predictive-load-dict 'dict-html-label)
  (predictive-load-dict 'dict-html-legend)
  (predictive-load-dict 'dict-html-link)
  (predictive-load-dict 'dict-html-map)
  (predictive-load-dict 'dict-html-meta)
  (predictive-load-dict 'dict-html-object)
  (predictive-load-dict 'dict-html-optgroup)
  (predictive-load-dict 'dict-html-option)
  (predictive-load-dict 'dict-html-param)
  (predictive-load-dict 'dict-html-script)
  (predictive-load-dict 'dict-html-select)
  (predictive-load-dict 'dict-html-style)
  (predictive-load-dict 'dict-html-table)
  (predictive-load-dict 'dict-html-td)
  (predictive-load-dict 'dict-html-textarea)
  (predictive-load-dict 'dict-html-tr)
  
  
  ;; add dictionary of html tags to active dictionaries
  (make-local-variable 'predictive-main-dict)
  (when (atom predictive-main-dict)
    (setq predictive-main-dict (list predictive-main-dict)))
  (add-to-list 'predictive-main-dict 'dict-html)
  
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-html-regexps
	(auto-overlay-init
	 '(
	   ;; "<!--" and "-->" delimit comments
	   (stack
	    (start "<!--" (dict . predictive-main-dict) (priority . 2)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (end "-->" (dict . predictive-main-dict) (priority . 2)))
	   
;; 	   ;; "<" starts an HTML tag, which ends at the next non-letter
;; 	   ;; character
;; 	   (word ("</?\\([[:alnum:]]*?\\)\\([^[:alnum:]]\\|$\\)" . 1)
;; 		 (dict . dict-html))
	   
	   ;; "<a" starts an anchor, ended by ">". "<" makes sure all other
	   ;; ">"s are matched
	   (stack
	    (start "<a " (dict . (list dict-html-a  dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<area " (dict . (list dict-html-area dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<base " (dict . dict-html-base) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<bdo "
		   (dict . (list dict-html-international dict-html-core))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<\\(blockquote\\|q\\) "
		   (dict . (list dict-html-quote dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<body " (dict . (list dict-html-body dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<br " (dict . dict-html-core) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<button " (dict . (list dict-html-button dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<col " (dict . (list dict-html-col dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<colgroup " (dict . (list dict-html-col dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<del " (dict . (list dict-html-del dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<form " (dict . (list dict-html-form dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<head "
		   (dict . (list dict-html-head dict-html-international))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<hr " (dict . (list dict-html-core dict-html-events))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<html " (dict . dict-html-international) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<img " (dict . (list dict-html-img dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<input " (dict . (list dict-html-input dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<ins " (dict . (list dict-html-ins dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<label " (dict . (list dict-html-label dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<legend " (dict . (list dict-html-legend dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<link " (dict . (list dict-html-link dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<map " (dict . (list dict-html-map dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<meta "
		   (dict . (list dict-html-meta dict-html-international))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<object " (dict . (list dict-html-object dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<optgroup "
		   (dict . (list dict-html-optgroup dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<option " (dict . (list dict-html-option dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<param " (dict . dict-html-param) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<script " (dict . dict-html-script) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<select " (dict . (list dict-html-select dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<style "
		   (dict . (list dict-html-style dict-html-international))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<table " (dict . (list dict-html-table dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<t\\(r\\|body\\|head\\|foot\\) "
		   (list dict-html-tr dict-html-common) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<t[dh] " (dict . (list dict-html-td dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<textarea "
		   (dict . (list dict-html-textarea dict-html-common))
		   (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<title " (dict . dict-html-international) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (start "<[[:alnum:]]+? " (dict . dict-html-common) (priority . 1)
		   (face . (background-color . ,predictive-html-debug-color)))
	    (end ">" (priority . 1)
		 (face . (background-color . ,predictive-html-debug-color))))
	   )))
  
  ;; make "<" and ">" do the right thing
  (setq completion-override-syntax-alist
	'((?< . (accept . (lambda () (complete "") nil)))
	  (?> . (accept . nil))))
  
  t  ; indicate succesful setup
)

;;; predictive-html.el ends here
