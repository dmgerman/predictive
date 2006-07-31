;;; auto-overlay-stack.el --- stacked start/end-delimited automatic overlays

;; Copyright (C) 2005 2006 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.1.1
;; Keywords: automatic, overlays, stack

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
;; Version 0.1.1
;; * bug fixes
;;
;; Version 0.1
;; * initial version separated off from auto-overlays.el



;;; Code:


(require 'auto-overlays)
(provide 'auto-overlay-stack)


;; register stack overlay parsing and suicide functions
(assq-delete-all 'stack auto-overlay-functions)
(push '(stack auto-o-parse-stack-match auto-o-stack-suicide)
      auto-overlay-functions)



(defun auto-o-parse-stack-match (o-match)
  ;; Perform any necessary updates of auto overlays due to a match for a stack
  ;; regexp.

  (let* ((overlay-stack (auto-o-stack o-match))
	 (o (car overlay-stack)))
    (cond
     ;; if the stack is empty, just create and return a new unmatched overlay
     ((null overlay-stack)
      (auto-o-make-stack o-match 'unmatched))
     
     ;; if appropriate edge of innermost overlay is unmatched, just match it
     ((or (and (eq (auto-o-edge o-match) 'start)
	       (not (auto-o-start-matched-p o)))
	  (and (eq (auto-o-edge o-match) 'end)
	       (not (auto-o-end-matched-p o))))
      (auto-o-match-overlay o o-match)
      ;; return nil since haven't created any new overlays
      nil)
     
     ;; otherwise...
     (t
      ;; create new innermost overlay and add it to the overlay stack
      (push (auto-o-make-stack o-match) overlay-stack)
      ;; sort out the overlay stack
      (auto-o-stack-cascade overlay-stack)
      ;; return newly created overlay
      (car overlay-stack)))
    )
)




(defun auto-o-stack-suicide (o-self)
  ;; Called when match no longer matches. Unmatch the match overlay O-SELF, if
  ;; necessary deleting its parent overlay or cascading the stack.
  
  (let* ((overlay-stack (auto-o-stack o-self))
	(o-parent (car overlay-stack)))
    
    ;; if parent is the only overlay in the stack...
    (if (= (length overlay-stack) 1)
	;; if we're a start match...
	(if (eq (auto-o-edge o-self) 'start)
	    (if (auto-o-end-matched-p o-parent)
		;; if parent is end-matched, make it start-unmatched
		(auto-o-match-overlay o-parent 'unmatched nil)
	      ;; if parent is end-unmatched delete it
	      (auto-o-delete-overlay o-parent))
	    
	  ;; if we're an end match...
	  (if (auto-o-start-matched-p o-parent)
	      ;; if parent is start matched, make it end-unmatched
	      (auto-o-match-overlay o-parent nil 'unmatched)
	    ;; if parent is start-unmatched, delete it
	    (auto-o-delete-overlay o-parent)))
      
      
      ;; otherwise, unmatch ourselves from parent and cascade the stack
      (overlay-put o-parent (auto-o-edge o-self) nil)
      (overlay-put o-self 'parent nil)
      (auto-o-stack-cascade overlay-stack)))
)

      


(defun auto-o-make-stack (o-match &optional unmatched)
  ;; Create a stack overlay for match overlay O-MATCH.
  ;; If UNMATCHED is nil, overlay will start and end at O-MATCH.
  ;; If non-nil, overlay will start or end from O-MATCH (depending on whether
  ;; O-MATCH is a 'start or 'end match) and stretch till end or beginning of
  ;; buffer.

  (let (o-new pos)
    ;; create new stack overlay and match it with O-MATCH
    (cond
     ((eq (auto-o-edge o-match) 'start)
      (setq pos (overlay-get o-match 'delim-end))
      (setq o-new (make-overlay pos pos nil nil 'rear-advance))
      (auto-o-match-overlay o-new o-match 'unmatched))
     
     ((eq (auto-o-edge o-match) 'end)
      (setq pos (overlay-get o-match 'delim-start))
      (setq o-new (make-overlay pos pos nil nil 'rear-advance))
      (auto-o-match-overlay o-new 'unmatched o-match)))

    ;; give the new overlay its basic properties
    (overlay-put o-new 'auto-overlay t)
    (overlay-put o-new 'set (overlay-get o-match 'set))
    (overlay-put o-new 'type (overlay-get o-match 'type))
    
    ;; return the new overlay
    o-new)
)



(defun auto-o-stack-cascade (overlay-stack)
  ;; Cascade the ends of the overlays in OVERLAY-STACK up or down the stack,
  ;; so as to re-establish a valid stack. It assumes that only the innermost
  ;; is incorrect.
  
  (let ((o (car overlay-stack)) o1)
    (cond
     
     ;; if innermost overlay is start-matched (and presumably
     ;; end-unmatched)...
     ((auto-o-start-matched-p o)
      ;; cascade overlay end matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(auto-o-match-overlay o nil
			      (if (overlay-get o1 'end)
				    (overlay-get o1 'end)
				'unmatched)
			      nil nil 'protect-match))
      ;; if final overlay is start-matched, make it end-unmatched, otherwise
      ;; delete it
      (if (auto-o-start-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (auto-o-match-overlay o1 nil 'unmatch nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match)))
     
     
     ;; if innermost overlay is end-matched (and presumably
     ;; start-unmatched)...
     ((auto-o-end-matched-p o)
      ;; cascade overlay start matches up through stack until one is left
      (dotimes (i (- (length overlay-stack) 1))
	(setq o (nth i overlay-stack))
	(setq o1 (nth (+ i 1) overlay-stack))
	(auto-o-match-overlay o (if (overlay-get o1 'start)
				    (overlay-get o1 'start)
				  'unmatched)
			      nil nil nil 'protect-match))
      ;; if final overlay is end-matched, make it start-unmatched, otherwise
      ;; delete it
      (if (auto-o-end-matched-p o1)
	  ;; FIXME: could postpone re-parsing here in case it can be avoided
	  (auto-o-match-overlay o1 'unmatch nil nil nil 'protect-match)
	(auto-o-delete-overlay o1 nil 'protect-match))))
    )
)




(defun auto-o-stack (o-match)
  ;; Return a list of the overlays that overlap and are of same type as match
  ;; overlay O-MATCH, ordered from innermost to outermost. (Assumes overlays
  ;; are correctly stacked.)
  
  ;; find overlays of same type overlapping O-MATCH
  (let ((overlay-stack (auto-overlays-at-point
			(if (eq (auto-o-edge o-match) 'start)
			    (overlay-get o-match 'delim-end)
			  (overlay-get o-match 'delim-start))
			(list '(eq auto-overlay t)
			      (list '= 'set (overlay-get o-match 'set))
			      (list '= 'type (overlay-get o-match 'type))))))
    ;; sort the list by overlay length, i.e. from innermost to outermose
    (sort overlay-stack
	  (lambda (a b)
	    (< (- (overlay-end a) (overlay-start a))
	       (- (overlay-end b) (overlay-start b))))))
)


;; auto-overlay-stack.el ends here
