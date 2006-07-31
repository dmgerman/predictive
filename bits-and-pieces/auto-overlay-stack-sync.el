
;;; auto-overlay-stack-sync.el --- syncronised stacked automatic overlays


;; Copyright (C) 2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: automatic, overlays, stack, sync
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is part of the Emacs Automatic Overlays package.
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
;; Version 0.1
;; * initial version



;;; Code:


(require 'auto-overlays)
(require 'auto-overlay-stack)
(provide 'auto-overlay-stack-sync)


;; register stack-sync overlay parsing and suicide functions
(assq-delete-all 'stack-sync auto-overlay-functions)
(push '(stack-sync auto-o-parse-stack-match auto-o-stack-suicide
		   auto-o-make-stack-sync-match)
      auto-overlay-functions)



(defun auto-o-make-stack-sync-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a
  ;; stack-sync regexp.

  ;; add sync function to end of modification-, insert-in-front- and
  ;; insert-behind-hooks (after suicide function)
  (overlay-put o-match 'modification-hooks
	       (append (overlay-get o-match 'modification-hooks)
		       '(auto-o-stack-sync-update)))
  (overlay-put o-match 'insert-in-front-hooks
	       (append (overlay-get o-match 'insert-in-front-hooks)
		       '(auto-o-stack-sync-update)))
  (overlay-put o-match 'insert-behind-hooks
	       (append (overlay-get o-match 'insert-behind-hooks)
		       '(auto-o-stack-sync-update)))
  ;; make sure new match overlay is synchronised
  (auto-o-stack-sync-update o-match t)
)



(defun auto-o-stack-sync-update (o-self modified &rest rest)
  ;; Syncronise start and end delimeters. Called by match overlay's
  ;; modification-hooks.
  
  (when modified
    (if (> auto-o-pending-suicide-count 0)
	(add-to-list 'auto-o-pending-post-suicide
		     (list 'auto-o-stack-sync-update o-self t) 'append)
      
      (let ((edge (if (eq (auto-o-edge o-self) 'start) 'end 'start))
	    o-parent o-stack o-other str)
	
	;; if match overlay is still in the buffer (it might have been been
	;; deleted after a suicide), has a parent, the parent is matched at
	;; the other end, and the entire stack is start and end matched...
	(when (and (overlay-buffer o-self)
		   (setq o-parent (overlay-get o-self 'parent))
		   (setq o-other (overlay-get o-parent edge))
		   (or (null (setq o-stack
				   (car (last (auto-o-stack o-self)))))
		       (and (overlay-get o-stack 'start)
			    (overlay-get o-stack 'end))))
	  (save-excursion
	    (save-match-data
	      ;; set match data for match overlay's regexp and get string to
	      ;; copy to other end
	      (goto-char (overlay-start o-self))
	      (when (looking-at (auto-o-regexp o-self))
		(setq str (match-string (auto-o-regexp-group-nth 1 o-self)))
		;; if string at other end doesn't match, replace it (it's
		;; important to check if it already matches or we get infinite
		;; recursion when it's own modification-hooks are called)
		(goto-char (overlay-start o-other))
		(when (and (looking-at (auto-o-regexp o-other))
			   (not (string=
				 str (match-string (auto-o-regexp-group-nth
						    1 o-other)))))
		  (let ((inhibit-modification-hooks t))
		    (replace-match str t t nil
				   (auto-o-regexp-group-nth 1 o-other)))))
	      )))
	)))
)
