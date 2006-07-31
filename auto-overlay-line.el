;;; auto-overlay-line.el --- automatic overlays for single lines

;; Copyright (C) 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.2
;; Keywords: automatic, overlays, line

;; This file is part of the Emacs Automatic Overlays package.
;;
;; The Emacs Automatic Overlays package is free software; you can
;; redistribute it and/or modify it under the terms of the GNU
;; General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; The Emacs Automatic Overlays package is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the Emacs Automatic Overlays package; if not, write
;; to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA


;;; Change Log:
;;
;; Version 0.2:
;; * got rid of fake end match overlays, which ensured the overlay always
;;   extended to end of line, in favour of adding a function to the
;;   modification hooks of the line overlay itself
;;
;; Version 0.1:
;; * initial version separated off from auto-overlays.el



;;; Code:


(require 'auto-overlays)
(provide 'auto-overlay-line)


;; register line overlay parsing and suicide funtions
(assq-delete-all 'line auto-overlay-functions)
(push (list 'line 'auto-o-parse-line-match
	    (lambda (o) (auto-o-delete-overlay (overlay-get o 'parent))))
      auto-overlay-functions)



(defun auto-o-parse-line-match (o-match)
  ;; Create overlay for a new line match.
  (let ((o-new (make-overlay (overlay-get o-match 'delim-end)
			     (save-excursion
			       (goto-char (overlay-get o-match 'delim-end))
			       (1+ (line-end-position))))))
    
    ;; give the new overlay its basic properties
    (overlay-put o-new 'auto-overlay t)
    (overlay-put o-new 'set (overlay-get o-match 'set))
    (overlay-put o-new 'type (overlay-get o-match 'type))
    ;; match start of new overlay with match
    (auto-o-match-overlay o-new o-match nil)
    ;; set overlay's modification hooks to ensure that it always extends to
    ;; end of line
    (overlay-put o-new 'modification-hooks
		 (cons 'auto-o-extend-line
		       (overlay-get o-new 'modification-hooks)))
    ;; return new overlay
    o-new)
)



(defun auto-o-extend-line (o-self modified &rest unused)
  ;; All line overlay modification hooks are set to this function, which
  ;; checks if overlay still extends to end of line, and updates the necessary
  ;; if not.

  ;; if we will be run after modification, increment pending suicide count to
  ;; avoid running `auto-overlay-update' until all suicides are done (this
  ;; isn't a suicide function, but we hook into the same mechanism anyway)
  (if (null modified)
      (setq auto-o-pending-suicide-count (1+ auto-o-pending-suicide-count))

    
    ;; if being run after modification, decrement pending suicide count
    (setq auto-o-pending-suicide-count (1- auto-o-pending-suicide-count))
    
    (save-match-data
      (let ((start (overlay-start o-self))
	    (end (overlay-end o-self)))
	(cond
	 ;; if we no longer extend to end of line...
	 ((null (string-match "\n" (buffer-substring-no-properties
				    (overlay-start o-self)
				    (overlay-end o-self))))
	  ;; grow ourselves so we extend till end of line
	  (move-overlay o-self start (save-excursion
				       (goto-char (overlay-end o-self))
				       (1+ (line-end-position))))
	  ;; if we're exclusive, delete lower priority overlays in newly
	  ;; covered region
	  (auto-o-update-exclusive (overlay-get o-self 'set)
				   end (overlay-end o-self)
				   nil (overlay-get o-self 'priority)))

	 
	 ;; if we extend beyond end of line...
	 ((/= (overlay-end o-self) (+ start (match-end 0)))
	  ;; shrink ourselves so we extend till end of line
	  (move-overlay o-self start (+ start (match-end 0)))
	  ;; if we're exclusive, re-parse region that is no longer covered
	  (auto-o-update-exclusive (overlay-get o-self 'set)
				   (overlay-end o-self) end
				   (overlay-get o-self 'priority) nil))
	 )))
    
    
    ;; if there are no more pending suicides and `auto-overlay-update' has
    ;; been postponed, run it now
    (when (and auto-o-pending-post-suicide (= auto-o-pending-suicide-count 0))
      (mapc (lambda (u) (apply (car u) (cdr u)))
	    auto-o-pending-post-suicide)
      (setq auto-o-pending-post-suicide nil)))
)
      
  
;; auto-overlay-line.el ends here
