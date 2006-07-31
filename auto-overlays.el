
;;; auto-overlays.el --- automatic regexp-delimited overlays for emacs


;; Copyright (C) 2005 2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.5
;; Keywords: automatic, overlays
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
;; Version 0.5
;; * changed the way suicide, update and other functions are called after a
;;   buffer modification: now called from `auto-o-run-after-change-functions'
;;
;; Version 0.4.2
;; * moved compatability code to auto-overlays-compat.el
;;
;; Version 0.4.1
;; * moved defmacros before their first use so byte-compilation works
;;
;; Version 0.4
;; * (a lot of) bug fixes
;;
;; Version 0.3
;; * completely re-written after realising that the match overlays, not the
;;   auto overlays themselves, should be the "primary" objects - much better!
;; * moved code for specific overlay types into separate files
;; * as a side effect, created a mechanism for defining new overlay types
;;   without modifying the auto-overlay code itself
;;
;; Version 0.2:
;; * added exclusive overlay support
;; * major code tidying and bug fixes
;;
;; Version 0.1
;; * initial version created by copying code from Predictive Completion
;;   package, with minor modifications



;;; Code:


(defvar auto-overlay-functions nil)
(defvar auto-overlay-regexps nil)
(make-variable-buffer-local 'auto-overlay-regexps)
(defvar auto-overlay-load-hook nil)
(defvar auto-overlay-unload-hook nil)


(require 'auto-overlay-common)
(provide 'auto-overlays)


(defvar auto-overlay-list nil)
(make-variable-buffer-local 'auto-overlay-list)
(defvar auto-o-pending-updates nil)
(make-variable-buffer-local 'auto-o-pending-updates)
(defvar auto-o-pending-suicides nil)
(make-variable-buffer-local 'auto-o-pending-suicides)
(defvar auto-o-pending-pre-suicide nil)
(make-variable-buffer-local 'auto-o-pending-pre-suicide)
(defvar auto-o-pending-post-suicide nil)
(make-variable-buffer-local 'auto-o-pending-post-suicide)
(defvar auto-o-pending-post-update nil)
(make-variable-buffer-local 'auto-o-pending-post-update)




;;;========================================================
;;;                 Code-tidying macros

(defmacro auto-o-entry (set type &optional sequence)
  ;; Return regexp entry corresponding to SET, TYPE and SEQUENCE.
  `(if ,sequence
       (nth (1+ ,sequence) (nth ,type (nth ,set auto-overlay-regexps)))
     (nth ,type (nth ,set auto-overlay-regexps))))


(defmacro auto-o-class (o-match)
  ;; Return class of match overlay O-MATCH.
  `(car (nth (overlay-get ,o-match 'type)
	     (nth (overlay-get ,o-match 'set) auto-overlay-regexps))))


(defmacro auto-o-seq-regexp (set type &optional sequence)
  ;; Return regexp corresponsing to SET, TYPE and SEQUENCE.
  `(let ((regexp (nth 1 (auto-o-entry ,set ,type ,sequence))))
     (if (atom regexp) regexp (car regexp))))


(defmacro auto-o-regexp (o-match)
  ;; Return match overlay O-MATCH's regexp.
  `(auto-o-seq-regexp (overlay-get ,o-match 'set)
		      (overlay-get ,o-match 'type)
		      (overlay-get ,o-match 'sequence)))


(defmacro auto-o-seq-regexp-group (set type &optional sequence)
  ;; Return regexp group corresponsing to SET, TYPE and SEQUENCE, or 0 if none
  ;; is specified.
  `(let ((regexp (nth 1 (auto-o-entry ,set ,type ,sequence))))
     (cond
      ((atom regexp) 0)
      ((atom (cdr regexp)) (cdr regexp))
      (t (cadr regexp)))))


(defmacro auto-o-regexp-group (o-match)
  ;; Return match overlay O-MATCH's regexp group.
  `(auto-o-seq-regexp-group (overlay-get ,o-match 'set)
			    (overlay-get ,o-match 'type)
			    (overlay-get ,o-match 'sequence)))


(defmacro auto-o-seq-regexp-group-nth (n set type &optional sequence)
  ;; Return Nth regexp group entry corresponsing to SET, TYPE and SEQUENCE, or
  ;; 0 if there is no Nth entry.
  `(let ((regexp (nth 1 (auto-o-entry ,set ,type ,sequence))))
     (cond
      ((atom regexp) 0)
      ((> (1+ ,n) (length (cdr regexp))) 0)
      (t (nth ,n (cdr regexp))))))


(defmacro auto-o-regexp-group-nth (n o-match)
  ;; Return match overlay O-MATCH's Nth regexp group entry, or 0 if there is
  ;; no Nth entry.
  `(auto-o-seq-regexp-group-nth ,n
				(overlay-get ,o-match 'set)
				(overlay-get ,o-match 'type)
				(overlay-get ,o-match 'sequence)))


(defmacro auto-o-type-props (set type &optional sequence)
  ;; Return properties of regexp with SET, TYPE and SEQUENCE
  `(if (auto-o-type-is-list-p ,set ,type)
       (nthcdr 2 (auto-o-entry ,set ,type ,sequence))
     (nthcdr 2 (auto-o-entry ,set ,type))))


(defmacro auto-o-props (o-match)
  ;; Return properties associated with match overlay O-MATCH.
  `(auto-o-type-props (overlay-get ,o-match 'set)
		      (overlay-get ,o-match 'type)
		      (overlay-get ,o-match 'sequence)))


(defmacro auto-o-seq-edge (set type sequence)
  ;; Return edge ('start or 'end) of regexp with SET, TYPE and SEQEUNCE
  ;; (assumes that TYPE contains a list of regexps)
  `(car (auto-o-entry ,set ,type ,sequence)))


(defmacro auto-o-edge (o-match)
  ;; Return edge ('start or 'end) of match overlay O-MATCH (assumes that
  ;; O-MATCH's type contains a list of regexps).
  `(auto-o-seq-edge (overlay-get ,o-match 'set)
		    (overlay-get ,o-match 'type)
		    (overlay-get ,o-match 'sequence)))


(defmacro auto-o-parse-function (o-match)
  ;; Return appropriate parse function for match overlay O-MATCH.
  `(nth 1 (assq (auto-o-class ,o-match) auto-overlay-functions)))


(defmacro auto-o-suicide-function (o-match)
  ;; Return appropriate suicide function for match overlay O-MATCH.
  `(nth 2 (assq (auto-o-class ,o-match) auto-overlay-functions)))


(defmacro auto-o-match-function (o-match)
  `(let ((funcs (assq (auto-o-class ,o-match) auto-overlay-functions)))
     (when (>= (length funcs) 4) (nth 3 funcs))))


(defmacro auto-o-edge-matched-p (overlay edge)
  ;; test if EDGE of OVERLAY is matched
  `(overlay-get ,overlay ,edge))


(defmacro auto-o-start-matched-p (overlay)
  ;; test if OVERLAY is start-matched
  `(overlay-get ,overlay 'start))


(defmacro auto-o-end-matched-p (overlay)
  ;; test if OVERLAY is end-matched
  `(overlay-get ,overlay 'end))


(defmacro auto-o-type-is-list-p (set type)
  ;; Return non-nil if regexp type TYPE contains a list of regexp entries
  ;; rather than a single entry.
  `(let ((entry (auto-o-entry ,set ,type 0)))
    (and (listp entry) (symbolp (car entry)))))




;;;=========================================================
;;;                auto-overlay functions

(defun auto-overlay-init (regexp-list &optional buffer)
  "Initialise a set of auto-overlays in BUFFER, or the current
buffer if none is specified, returning an identifier that can be
used to clear the overlays in the set by a call to
`auto-overlay-clear'. Since this identifier is the only means to
clear the overlay set later, the return value of
`auto-overlay-init' should usually be saved.

REGEXP-LIST must be a list with elements in one of the following
forms:

  (CLASS REGEXP @rest PROPS)

  (CLASS (EDGE REGEXP @rest PROPS) (EDGE REGEXP @rest PROPS) ...)

CLASS is a symbol which defines the behaviour of any overlays
created by matches to the regular expression REGEXP. The inbuilt
classes are: `word', `line', `self', `stack'. The first form
should be used for classes that only require one delimiter to
define an overlay, the second for classes that require start and
end delimiters. The inbuilt `word', `line' and `self' classes
require the first form, whereas `stack' requires the second.

Each PROPS element should be a list of the form (PROP . VALUE).
Each entry specifies an overlay property PROP (a symbol), and a
VALUE for that property. Overlays created by matches to REGEXP
acquire those properties.

For classes with start and end delimiters, EDGE should be one of
`start' or `end'. The order of the entries defines which match is
used if two regexps for the same EDGE match overlapping text:
whichever comes first in the list takes precedence. Similarly,
when an overlay is matched with both `start' and `end'
delimiters, it acquires the properties of whichever comes first
in the list.

Usually, each entry in REGEXP-LIST acts independently; the
different overlays they define have no influence on
eachother. However, if an overlay is given a non-nil `exclusive'
property, it prevents matches for any regexps with lower a
`priority' property within the region it covers. A null
`priority' is considered lower than any explicitly set
`priority'.

Multiple calls to `auto-overlay-init' set up separate sets of
overlays, which act completely independently, and can be
individually removed by calling `auto-overlay-clear' with the
appropriate identifier."

  (save-excursion
    (when buffer (set-buffer buffer))
    
    ;; add regexp definitions
    (push regexp-list auto-overlay-regexps)
    ;; create auto overlay slots for all the types defined by regexp-list
    (push (make-list (length regexp-list) nil) auto-overlay-list)
    
    
    ;; when auto-overlays haven't been activated before in this buffer...
    (when (= (length auto-overlay-regexps) 1)
      ;; run initialisation hooks
      (run-hooks 'auto-overlay-load-hook)
      ;; add hook to schedule an update after a buffer modification
      (add-hook 'after-change-functions 'auto-o-schedule-update nil t)
      ;; add hook to runs all the various functions scheduled be run after a
      ;; buffer modification
      (add-hook 'after-change-functions 'auto-o-run-after-change-functions
		nil t)
;;       ;; reset pending-suicide-count before updates to work around bug(?) that
;;       ;; overlay modification-hooks are not always called after modification
;;       (add-hook 'before-change-functions
;; 		(lambda (&rest ignore) (setq auto-o-pending-suicide-count 0))
;; 		nil t)
      )
    
    
    ;; search for new auto overlays
    (let ((lines (count-lines (point-min) (point-max)))
	  (set (1- (length auto-overlay-regexps))))
      (goto-char (point-min))
      (message "Scanning for auto-overlays...(line 1 of %d)"
	       lines)
      (dotimes (i lines)
	(when (= 9 (mod i 10))
	  (message
	   "Scanning for auto-overlays...(line %d of %d)"
	   (+ i 1) lines))
	(auto-overlay-update nil nil set)
	(forward-line 1))
      (message "Scanning for auto-overlays...done")
      
      ;; return overlay set identifier to use in calls to `auto-overlay-clear'
      set))
)




(defun auto-overlay-clear (set &optional buffer)
  "Clear all auto-overlays in the set identified by SET (as
returned by the call to `auto-overlay-init' that created them)
from BUFFER, or the current buffer if none is specified."

  (save-excursion
    (when buffer (set-buffer buffer))
    
    ;; delete overlays
    (mapc 'delete-overlay
	  (auto-overlays-in (point-min) (point-max)
			    (list
			     (list (lambda (overlay match) (or overlay match))
				   '(auto-overlay auto-overlay-match))
			     (list '= 'set set))
			    nil 'inactive))
    ;; remove overlays from list
    (setq auto-overlay-list
	  (delq (nth set auto-overlay-list) auto-overlay-list))
    ;; remove regexp definitions
    (setq auto-overlay-regexps
	  (delq (nth set auto-overlay-regexps) auto-overlay-regexps))

    
    ;; if there are no more active auto-overlay definitions...
    (unless auto-overlay-regexps
      ;; run clear hooks
      (run-hooks 'auto-overlay-unload-hook)
      ;; reset variables
      (remove-hook 'before-change-functions 'auto-o-schedule-update t)
      (remove-hook 'before-change-functions
		   'auto-o-run-after-change-functions)
;;       (remove-hook 'before-change-functions
;; 		   (lambda (&rest ignore)
;; 		     (setq auto-o-pending-suicide-count 0)) t)
      (setq auto-o-pending-suicides nil
	    auto-o-pending-updates nil
	    auto-o-pending-post-suicide nil))
    )
)




(defun auto-o-run-after-change-functions (start end unused)
  ;; Assigned to the `after-change-functions' hook. Run all the various
  ;; functions that should run after a change to the buffer, in the correct
  ;; order.

  ;; run pending pre-suicide functions
  (when auto-o-pending-pre-suicide
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-pre-suicide)
    (setq auto-o-pending-pre-suicide nil))  
  ;; run pending suicides
  (when auto-o-pending-suicides
    (mapc (lambda (o) (funcall 'auto-o-suicide o)) auto-o-pending-suicides)
    (setq auto-o-pending-suicides nil))
  ;; run pending post-suicide functions
  (when auto-o-pending-post-suicide
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-post-suicide)
    (setq auto-o-pending-post-suicide nil))
  ;; run updates
  (when auto-o-pending-updates
    (mapc (lambda (l) (apply 'auto-overlay-update l)) auto-o-pending-updates)
    (setq auto-o-pending-updates nil))
  ;; run pending post-update functions
  (when auto-o-pending-post-update
    (mapc (lambda (f) (apply (car f) (cdr f))) auto-o-pending-post-update)
    (setq auto-o-pending-post-update nil))
)



(defun auto-o-schedule-update (start &optional end unused regexp-set)
  ;; Schedule `auto-overlay-update' of lines between positions START and END
  ;; (including lines containing START and END), optionally restricted to
  ;; REGEXP-SET. If END is not supplied, schedule update for just line
  ;; containing START. The update will be run by
  ;; `auto-o-run-after-change-functions' after buffer modification is
  ;; complete. This function is assigned to `after-change-functions'.

  ;; FIXME: we should do more to avoid doing multiple, redundant
  ;;        updates. Currently, only updates for identical regions are
  ;;        filtered, not updates for overlapping regions.
  (add-to-list 'auto-o-pending-updates
	       (list (line-number-at-pos start)
		     (when end (line-number-at-pos end))
		     regexp-set))
)



(defun auto-o-schedule-suicide (o-self modified &rest unused)
  ;; Schedule `auto-o-suicide' to run after buffer modification is
  ;; complete. It will be run by `auto-o-run-after-change-functions'. Assigned
  ;; to overlay modification and insert in-front/behind hooks.
  (unless modified (add-to-list 'auto-o-pending-suicides o-self))
)



(defun auto-overlay-update (&optional start end regexp-set)
  ;; Parse lines from line number START to line number END. If only START is
  ;; supplied, just parse that line. If neither are supplied, parse line
  ;; containing the point. If REGEXP-SET is specified, only look for matches
  ;; in that set of overlay regexps definitions.
  
  (let (regexp-list class regexp group priority set sequence
		    o-match o-overlap o-new)
    (unless start (setq start (line-number-at-pos)))
    (save-excursion
      (save-match-data
	(goto-line start)
	(dotimes (i (if end (1+ (- end start)) 1))
	  
	  ;; check each set of overlays, unless specific set was specified
	  (dotimes (s (if regexp-set 1 (length auto-overlay-regexps)))
	    (if regexp-set (setq set regexp-set) (setq set s))
	    ;; check each type of auto overlay
	    (dotimes (type (length (nth set auto-overlay-regexps)))
	      (setq regexp-list (nth type (nth set auto-overlay-regexps)))
	      (setq class (nth 0 regexp-list))
	      (if (auto-o-type-is-list-p set type)
		  (pop regexp-list)	; remove class to leave regexp list
		(setq regexp-list (list regexp-list))) ; bundle in list
		
	      ;; check all regexps for current type
	      (dotimes (seq (length regexp-list))
		(if (> (length regexp-list) 1)
		    (setq sequence seq)
		  (setq sequence nil))
		  
		;; extract regexp properties from current entry
		(setq regexp (auto-o-seq-regexp set type sequence))
		(setq group (auto-o-seq-regexp-group set type sequence))
		(setq priority
		      (cdr (assq 'priority
				 (auto-o-type-props set type sequence))))
		  
		  
		;; look for matches in current line
		(forward-line 0)
		(while (re-search-forward regexp (line-end-position) t)
		  (cond
		   ;; ignore match if it already has a match overlay
		   ((auto-o-matched-p (match-beginning 0) (match-end 0)
				      set type sequence))
		     
		     
		   ;; if existing match overlay of same type and edge but
		   ;; different sequence overlaps the new match...
		   ((and (auto-o-type-is-list-p set type)
			 (setq o-overlap
			       (auto-o-overlapping-match
				(match-beginning group) (match-end group)
				set type sequence
				(auto-o-seq-edge set type sequence))))
		    ;; if new match takes precedence, replace existing one
		    ;; with new one, otherwise ignore new match
		    (when (< sequence (overlay-get o-overlap 'sequence))
		      (delete-overlay o-overlap)
		      (setq o-match (auto-o-make-match
				     set type
				     (match-beginning 0) (match-end 0)
				     sequence (match-beginning group)
				     (match-end group)))
		      (when (overlay-get o-overlap 'parent)
			(auto-o-match-overlay (overlay-get o-overlap 'parent)
					      o-match))
		      ;; run match function if there is one
		      (let ((match-func (auto-o-match-function o-match)))
			(when match-func (funcall match-func o-match)))))
		     
		   ;; if match is within a higher priority exclusive
		   ;; overlay, create match overlay but don't parse it
		   ((auto-o-within-exclusive-p (match-beginning group)
					       (match-end group)
					       priority)
		    (auto-o-make-match set type
				       (match-beginning 0) (match-end 0)
				       sequence (match-beginning group)
				       (match-end group)))
		     
		     
		   ;; if we're going to parse the new match...
		   (t
		    ;; create a match overlay for it
		    (setq o-match (auto-o-make-match
				   set type
				   (match-beginning 0) (match-end 0)
				   sequence
				   (match-beginning group)
				   (match-end group)))
		    ;; call the appropriate parse function
		    (setq o-new
			  (funcall (auto-o-parse-function o-match) o-match))
		    (unless (listp o-new) (setq o-new (list o-new)))
		    ;;  and add any new overlays to `auto-overlay-list' and
		    ;;  give them appropriate properties
		    (mapc (lambda (o)
			    (setcar (nthcdr type
					    (nth set auto-overlay-list))
				    (cons
				     o (nth type
					    (nth set auto-overlay-list))))
			    (overlay-put o 'auto-overlay t)
			    (overlay-put o 'set set)
			    (unless (overlay-get o 'type)
			      (overlay-put o 'type type)))
			  o-new)
		    ;; run match function if there is one
		    (let ((match-func (auto-o-match-function o-match)))
		      (when match-func (funcall match-func o-match)))))
		    
		    
		  ;; go to character one beyond the start of the match, to
		  ;; make sure we don't miss the next match (if we find the
		  ;; same one again, it will just be ignored)
		  (goto-char (+ (match-beginning 0) 1)))))
	    (forward-line 1))
	  ))))
)




(defun auto-o-suicide (o-self)
  ;; This function is assigned to all match overlay modification hooks, and
  ;; calls the appropriate suicide function for match overlay O-SELF as
  ;; specified in `auto-overlay-functions'.
  
  ;; this is here to avoid a weird bug(?) where the modification-hooks seem
  ;; to be called occasionally for overlays that have already been deleted
  (when (overlay-buffer o-self)
    ;; if match overlay no longer matches the text it covers...
    (unless (and (save-excursion
		  (goto-char (overlay-start o-self))
		  (looking-at (auto-o-regexp o-self)))
		 (= (match-end 0) (overlay-end o-self)))
      ;; if we have a parent overlay, call appropriate suicide function,
      ;; schedule an update (necessary for complicated reasons!) then delete
      ;; ourselves
      (when (overlay-get o-self 'parent)
	(funcall (auto-o-suicide-function o-self) o-self))
      ;; Note: not supplying the 'set can avoid multiple, effectively
      ;; identical auto-overlay-update calls
      (auto-o-schedule-update (overlay-start o-self))
      (delete-overlay o-self)))
)




(defun auto-o-update-exclusive (set beg end old-priority new-priority)
  ;; If priority has increased, delete all overlays between BEG end END that
  ;; have priority lower than NEW-PRIORITY. If priority has decreased, re-parse
  ;; all matches with priority lower than OLD-PRIORITY.

  (let (overlay-list)
    (cond
     ;; if priority has increased...
     ((and new-priority
	   (or (null old-priority) (> new-priority old-priority)))
      ;; find overlays entirely within BEG and END that are both start and end
      ;; matched and have priority lower than NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay)
		   (list '= 'set set)
		   '(identity start)
		   (list (lambda (type start end)
			   (or (null (auto-o-type-is-list-p set type))
			       (and start end)))
			 '(type start end))
		   (list (lambda (pri new) (or (null pri) (< pri new)))
			 'priority new-priority))
	     'within))
      ;; mark overlays in list as inactive (more efficient than calling
      ;; suicide functions or deleting the overlays, and leaves them intact in
      ;; case the exclusivity of the region is later reduced - see below)
      (dolist (o overlay-list) (overlay-put o 'inactive t))
      
      ;; find match overlays between BEG and END that have priority lower then
      ;; NEW-PRIORITY but still have an active parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay-match)
		   (list '= 'set set)
		   (list (lambda (parent)
			   (null (overlay-get parent 'inactive)))
			 'parent)
		   (list (lambda (pri new) (or (null pri) (< pri new)))
			 'priority new-priority))))
      ;; call appropriate suicide function for each match overlay in list
      (dolist (o overlay-list) (funcall (auto-o-suicide-function o) o)))
     
     
     ;; if priority has decreased...
     ((and old-priority
	   (or (null new-priority) (< new-priority old-priority)))
      ;; find inactive overlays entirely within BEG and END that have priority
      ;; higher or equal to NEW-PRIORITY
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay)
		   (list '= 'set set)
		   '(identity inactive)
		   (list (lambda (pri new) (or (null new) (>= pri new)))
			 'priority new-priority))
	     'within 'inactive))
      ;; mark overlays in list as active again
      (dolist (o overlay-list) (overlay-put o 'inactive nil))
      
      ;; find match overlays between BEG and END that have priority higher or
      ;; equal to NEW-PRIORITY but no parent overlay
      (setq overlay-list
	    (auto-overlays-in
	     beg end
	     (list '(identity auto-overlay-match)
		   (list '= 'set set)
		   '(null parent)
		   (list (lambda (pri new) (or (null new) (>= pri new)))
			 'priority new-priority))))
      ;; call appropriate parse function for each match overlay in list
      (dolist (o-match overlay-list)
	(when (not (auto-o-within-exclusive-p o-match))
	  (let ((o-new (funcall (auto-o-parse-function o-match) o-match)))
	    ;;  and add any new overlays to `auto-overlay-list' and give them
	    ;;  appropriate properties
	    (unless (listp o-new) (setq o-new (list o-new)))
	    (mapc (lambda (o)
		    (setcar (nthcdr (overlay-get o 'type)
				    (nth set auto-overlay-list))
			    (cons o (nth (overlay-get o 'type)
					 (nth set auto-overlay-list))))
		    (overlay-put o 'auto-overlay t)
		    (overlay-put o 'set set)
		    (unless (overlay-get o 'type)
		      (overlay-put o 'type (overlay-get o-match 'type))))
		  o-new)))))
     ))
)




(defun auto-o-make-match (set type start end
			      &optional sequence delim-start delim-end)
  ;; Create a new match overlay and give it the appropriate properties.
  (let ((o-match (make-overlay start end nil 'front-advance nil)))
    (overlay-put o-match 'auto-overlay-match t)
    (overlay-put o-match 'set set)
    (overlay-put o-match 'type type)
    (overlay-put o-match 'delim-start
		 (set-marker (make-marker)
			     (if delim-start delim-start start)))
    (overlay-put o-match 'delim-end
		 (set-marker (make-marker)
			     (if delim-end delim-end end)))
    (set-marker-insertion-type (overlay-get o-match 'delim-start) t)
    (set-marker-insertion-type (overlay-get o-match 'delim-end) nil)
    (overlay-put o-match 'modification-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-in-front-hooks '(auto-o-schedule-suicide))
    (overlay-put o-match 'insert-behind-hooks '(auto-o-schedule-suicide))
    ;; when regexp entry is a list of regexps, store sequence property
    (when (auto-o-type-is-list-p set type)
      (overlay-put o-match 'sequence sequence))
    ;; return the new match overlay
    o-match)
)




(defun auto-o-match-overlay (overlay start &optional end
				     no-props no-parse protect-match)
  "Match start and end of OVERLAY with START and END match overlays.
If START or END are numbers or markers, move that edge to the
buffer location specified by the number or marker and make it
unmatched.  If START or END are non-nil but neither of the above,
make that edge unmatched.  If START or END are null, don't change
that edge. However, if END is null, and START is an 'end overlay,
match end of OVERLAY rather than start.
  
If NO-PARSE is non-nil, block re-parsing due to exclusive overlay
changes. If NO-PROPS is non-nil, block updating of overlay's
properties. If PROTECT-MATCH is non-nil, don't modify any match
overlays associated with OVERLAY (i.e. don't modify their 'parent
properties)."
  
  (let ((old-start (overlay-start overlay))
	(old-end (overlay-end overlay))
	(old-exclusive (overlay-get overlay 'exclusive))
	(old-priority (overlay-get overlay 'priority)))
    
    ;; if END is null, we're not unmatching, and START is an end overlay,
    ;; match end of overlay instead of start (Note: assumes we're matching an
    ;; overlay type with 'start and 'end regexps)
    (when (and (null end) (overlayp start) (eq (auto-o-edge start) 'end))
      (setq end start)
      (setq start nil))
    
    
    ;; move overlay to new location
    (move-overlay overlay
		  (cond
		   ((overlayp start) (overlay-get start 'delim-end))
		   ((number-or-marker-p start) start)
		   (start (point-min))
		   (t (overlay-start overlay)))
		  (cond
		   ((overlayp end) (overlay-get end 'delim-start))
		   ((number-or-marker-p end) end)
		   (end (point-max))
		   (t (overlay-end overlay))))
    ;; sort out start and end properties
    (let (o-match)
      ;; if unmatching start...
      (when (and start (not (overlayp start)))
	(setq o-match (overlay-get overlay 'start))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'start nil))
      ;; if unmatching end...
      (when (and end (not (overlayp end)))
	(setq o-match (overlay-get overlay 'end))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'end nil))
      ;; if matching start...
      (when (overlayp start)
	(setq o-match (overlay-get overlay 'start))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'start start)
	(overlay-put start 'parent overlay))
      ;; if matching end...
      (when (overlayp end)
	(setq o-match (overlay-get overlay 'end))
	(when (and o-match (null protect-match))
	  (overlay-put o-match 'parent nil))
	(overlay-put overlay 'end end)
	(overlay-put end 'parent overlay)))
    
    
    ;; unless it's blocked, update properties if new match takes precedence
    ;; (Note: this sometimes sets the overlay's properties to the ones it
    ;; already had, but it hardly seems worth checking for that)
    (unless no-props
      (let (props)
	(cond
	 ;; if start has been unmatched, use properties of end match
	 ((null (overlay-get overlay 'start))
	  (setq props (auto-o-props (overlay-get overlay 'end))))
	 ;; if end has been unmatched, use properties of start match
	 ((null (overlay-get overlay 'end))
	  (setq props (auto-o-props (overlay-get overlay 'start))))
	 (t  ;; otherwise, use properties of whichever match takes precedence
	  (let ((o-start (overlay-get overlay 'start))
		(o-end (overlay-get overlay 'end)))
	    (if (< (overlay-get o-start 'sequence)
		   (overlay-get o-end 'sequence))
		(setq props (auto-o-props o-start))
	      (setq props (auto-o-props o-end))))))
	;; bundle properties inside a list if not already, then update them
	(when (symbolp (car props)) (setq props (list props)))
	(dolist (p props) (overlay-put overlay (car p) (cdr p)))))
    
    
    ;; unless it's blocked or overlay is inactive, check if anything needs
    ;; reparsing due to exclusive overlay changes
    (unless (or no-parse (overlay-get overlay 'inactive))
      (let ((set (overlay-get overlay 'set))
	    (start (overlay-start overlay))
	    (end (overlay-end overlay))
	    (exclusive (overlay-get overlay 'exclusive))
	    (priority (overlay-get overlay 'priority)))
	(cond
	 
	;; if overlay wasn't and still isn't exclusive, do nothing
	 ((and (null exclusive) (null old-exclusive)))
	 
	 ;; if overlay has become exclusive, delete lower priority overlays
	 ;; within it
	 ((and (null old-exclusive) exclusive)
	  (auto-o-update-exclusive set start end nil priority))
	 
	 ;; if overlay was exclusive but no longer is, re-parse region it
	 ;; used to cover
	 ((and old-exclusive (null exclusive))
	  (auto-o-update-exclusive set old-start old-end old-priority nil))
	 
	 ;; if overlay was and is exclusive, and has been moved to a
	 ;; completely different location re-parse old location and delete
	 ;; lower priority overlays within new location
	 ((or (< end old-start) (> start old-start))
	  (auto-o-update-exclusive set start end old-priority nil)
	  (auto-o-update-exclusive set start end nil priority))

	 ;; if overlay was and is exclusive, and overlaps its old location...
	 (t
	  ;; if priority has changed, re-parse/delete in overlap region
	  (when (/= old-priority priority)
	    (auto-o-update-exclusive set
				     (max start old-start) (min end old-end)
				     old-priority priority))
	  (cond
	   ;; if overlay was exclusive and start has shrunk, re-parse
	   ;; uncovered region
	   ((and (> start old-start) old-exclusive)
	    (auto-o-update-exclusive set old-start start old-priority nil))
	   ;; if overlay is exclusive and has grown, delete lower priority
	   ;; overlays in newly covered region
	   ((and (< start old-start) exclusive)
	    (auto-o-update-exclusive set start old-start nil priority)))
	  (cond
	   ;; if overlay was exclusive and end has shrunk, re-parse
	   ((and (< end old-end) old-exclusive)
	    (auto-o-update-exclusive set end old-end old-priority nil))
	    ;; if overlay is exclusive and has grown, delete lower priority
	   ((and (> end old-end) exclusive)
	    (auto-o-update-exclusive set old-end end nil priority))))
	 )))
    )
)




(defun auto-o-delete-overlay (overlay &optional no-parse protect-match)
  ;; Delete OVERLAY from buffer and `auto-overlay-list'. If PROTECT-MATCH is
  ;; non-nil, don't modify any match overlays associated with OVERLAY
  ;; (i.e. leave their 'parent properties alone). If NO-PARSE is non-nil,
  ;; block re-parsing due to exclusive overlay changes.
  
  (let ((start (overlay-start overlay))
	(end (overlay-end overlay))
	o-match)
    ;; delete overlay from buffer and `auto-overlay-list'
    (delete-overlay overlay)
    (unless (setq o-match (overlay-get overlay 'start))
      (setq o-match (overlay-get overlay 'end)))
    (setcar (nthcdr (overlay-get o-match 'type)
		    (nth (overlay-get o-match 'set) auto-overlay-list))
	    (delq overlay (nth (overlay-get o-match 'type)
			       (nth (overlay-get o-match 'set)
				    auto-overlay-list))))
    
    ;; unless blocked, if overlay's exclusive flag was set, re-parse region it
    ;; covered
    (when (and (null no-parse) (overlay-get overlay 'exclusive))
      (auto-o-update-exclusive (overlay-get overlay 'set) start end
			       (overlay-get overlay 'priority) nil))
    
    ;; Note: it's vital that the match overlays' parent properties are only
    ;; set to nil *after* `auto-update-exclusive' is run: if the overlay
    ;; overlapped one of its match overlays, the newly parentless match
    ;; overlay would be re-parsed by `auto-update-exclusive', which would
    ;; re-create the parent overlay that's just been deleted!
    
    ;; unmatch match overlays
    (unless protect-match
      (when (setq o-match (overlay-get overlay 'start))
	(overlay-put o-match 'parent nil))
      (when (setq o-match (overlay-get overlay 'end))
	(overlay-put o-match 'parent nil)))
    )
)




(defun auto-o-matched-p (beg end set type &optional sequence)
  ;; Determine if characters between BEG end END are already matched by a
  ;; match overlay from set SET of type TYPE and optionally sequence SEQUENCE.
  (let (o-match)
    (catch 'match
      (mapc (lambda (o)
	      (when (and (overlay-get o 'auto-overlay-match)
			 (= (overlay-get o 'set) set)
			 (= (overlay-get o 'type) type)
			 (or (not (auto-o-type-is-list-p set type))
			     (= (overlay-get o 'sequence) sequence))
			 (= (overlay-start o) beg)
			 (= (overlay-end o) end))
		(setq o-match o)
		(throw 'match t)))
	    (overlays-in beg end)))
    o-match)
)




(defun auto-o-within-exclusive-p (match &optional end priority)
  ;; If MATCH is an overlay, determine if it is within a higher priority
  ;; exclusive overlay. If MATCH is a number or marker, determine whether
  ;; region between MATCH and END is within an exclusive overlay with higher
  ;; priority than PRIORITY.

  (when (null end)
    (setq end (overlay-get match 'delim-end))
    (setq priority (overlay-get match 'priority))
    (setq match (overlay-get match 'delim-start)))
  
  ;; look for higher priority exclusive overlays
  (auto-overlays-in
   match end
   (list '(identity auto-overlay)
	 '(identity exclusive)
	 (list (lambda (p q) (and p (or (null q) (> p q))))
	       'priority priority)))
)
  



(defun auto-o-overlapping-match (beg end set type sequence edge)
  ;; Returns any match overlay of same SET, TYPE and EDGE but different
  ;; SEQUENCE whose delimeter overlaps region from BEG to END. (Only returns
  ;; first one it finds; which is returned if more than one exists is
  ;; undefined.)
  (let (o-overlap)
    (catch 'match
      (mapc (lambda (o)
	      (when (and (overlay-get o 'auto-overlay-match)
			 (= (overlay-get o 'set) set)
			 (= (overlay-get o 'type) type)
			 (/= (overlay-get o 'sequence) sequence)
			 (eq (auto-o-edge o) edge)
			 ;; check delimeter (not just o) overlaps BEG to END
			 (<= (overlay-get o 'delim-start) end)
			 (>= (overlay-get o 'delim-end) beg))
		(setq o-overlap o)
		(throw 'match t)))
	    (overlays-in beg end)))
    o-overlap)
)




;;; ===============================================================
;;;                       Compatibility Stuff

(unless (fboundp 'line-number-at-pos)
  (require 'auto-overlays-compat)
  (defalias 'line-number-at-pos
            'auto-overlays-compat-line-number-at-pos)
)


;;; auto-overlays.el ends here
