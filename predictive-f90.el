
;;; predictive-setup-f90.el --- predictive mode Fortran 90 setup function


;; Copyright (C) 2005 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2
;; Keywords: predictive, setup function, fortran, f90
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
;; Version 0.2
;; * modified to use new auto-overlays package
;;
;; Version 0.1
;; * initial release



;;; Code:

(require 'predictive)
(provide 'predictive-f90)

;; variable to store identifier from call to `auto-overlay-init'
(defvar predictive-f90-regexps nil)
(make-local-variable 'predictive-f90-regexps)


(defun predictive-setup-f90 ()
  "Sets up predictive mode for use with Fortran 90 mode."
  (interactive)
  
  ;; load the dictionaries
  (predictive-load-dict 'dict-english)
  (predictive-load-dict 'dict-f90)
  (predictive-load-dict 'dict-f90-attributes)
  (predictive-load-dict 'dict-f90-intent)
  (predictive-load-dict 'dict-f90-open)
  (predictive-load-dict 'dict-f90-close)
  (predictive-load-dict 'dict-f90-read)
  (predictive-load-dict 'dict-f90-write)
  (predictive-load-dict 'dict-f90-rewind-backspace)

  ;; clear overlays when predictive mode is disabled
  (add-hook 'predictive-mode-disable-hook
	    (lambda () (auto-overlay-clear predictive-f90-regexps)))
    
  ;; use Fortran 90 dictionary
  (set (make-local-variable 'predictive-main-dict) 'dict-f90)
  
  ;; user-defined procedure and variable names are added to buffer dictionary
  (set (make-local-variable 'predictive-auto-add-to-dict) 'buffer)
  (set (make-local-variable 'predictive-add-to-dict-ask) nil)
  
  ;; setup regexps defining switch-dict regions
  (setq predictive-f90-regexps
	(auto-overlay-init
	 '(
	   ;; ! starts a comment, which lasts till end of line
	   (line "!" (dict . dict-english) '(priority . 3))
	   
	   ;; 's and "s delimit the start and end of character constants
	   ;; (don't care that "" is escaped, since behaves correctly like
	   ;; this anyway)
	   (self "\"" (dict . dict-english) '(priority . 2))
	   (self "'" (dict . dict-english) '(priority . 2))
	   
	   ;; attributes come after type declarations but before "::"
	   (stack
	    (start "character" (dict . dict-f90-attributes))
	    (start "complex" (dict . dict-f90-attributes))
	    (start "double precision" (dict . dict-f90-attributes))
	    (start "integer" (dict . dict-f90-attributes))
	    (start "logical" (dict . dict-f90-attributes))
	    (start "real" (dict . dict-f90-attributes))
	    (end "::" (dict . il)))
	   
	   ;; keywords and named arguments
	   (stack
	    ;; keywords inside Intent
	    (start "intent\\s *?(" (dict . dict-f90-intent))
	    ;; named arguments inside Open
	    (start "open\\s *?(" (dict . dict-f90-open))
	    ;; named arguments inside Close
	    (start "close\\s *?(" (dict . dict-f90-close))
	    ;; named arguments inside Read
	    (start "read\\s *?(" (dict . dict-f90-read))
	    ;; named arguments inside Write
	    (start "write\\s *?(" (dict . dict-f90-write))
	    ;; named arguments inside Rewind
	    (start "rewind\\s *?(" (dict . dict-f90-rewind-backspace))
	    ;; named arguments inside Backspace
	    (start "backspace\\s *?(" (dict . dict-f90-rewind-backspace))
	    ;; make sure all ('s are accounted for
	    (start "(")
	    (end ")"))
	   )))
  
  t  ; indicate succesful setup
)


;;; predictive-f90.el ends here
