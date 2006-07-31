
;;; predictive-setup-java.el --- predictive mode LaTeX setup function


;; Copyright (C) 2004 Nascif Abousalh-Neto

;; Author: Nascif Abousalh-Neto
;; Version: 0.1
;; Keywords: predictive, setup function, java

;; This file is part of the Emacs Predictive Completion package.
;;
;; The Emacs Predicive Completion package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Predicive Completion package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Predicive Completion package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Change Log:
;;
;; version 0.1: initial release (slight modification by Toby Cubitt for
;;              packaging)



;;; Code:

(defun predictive-setup-java ()
  "Sets up predictive mode for use with java major modes."
  (interactive)
  (require 'dict-java)
  (set (make-local-variable 'predictive-main-dict) 'dict-java)
  (set (make-local-variable 'predictive-switch-dict-regexps)
       (list
        (list "\"" 'self 'dict-english '(priority . 1))))
  t ; indicate succesful setup
)

;;; predictive-setup-java.el ends here
