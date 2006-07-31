
;;; predictive-setup-c.el --- predictive mode c setup function


;; Copyright (C) 2004 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.1
;; Keywords: predictive, setup function, c

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
;; version 0.1: initial release



;;; Code:

(require 'predictive)
(provide 'predictive-setup-c)


(defun predictive-setup-c ()
  "Sets up predictive mode for use with c major modes."
  (interactive)
  
  ;; use the c dictionary
  (require 'dict-c)
  (set (make-local-variable 'predictive-main-dict) 'dict-c)
  
  ;; add words to the buffer dictionary, since new words are probably
  ;; variable or function names
  (setq predictive-auto-add-to-dict 'buffer)
  t
)

;;; predictive-setup-c.el ends here
