
;;; completion-ui.el --- in-buffer completion user interface


;; Copyright (C) 2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.2.1
;; Keywords: completion, ui, user interface
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
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


;;; Commentary:
;;
;; This package provides user-interface functions for in-buffer
;; text completion. It doesn't find completions itself. Instead, a
;; completion program can simply call the `complete' function, passing
;; it the completion prefix and a list of completion candidates, and
;; this package does the rest!
;;
;; Typically, a lot of code in packages providing some kind of text
;; completion deals with the user interface. The goal is that all
;; packages providing in-buffer (and possibly also mini-buffer)
;; completion should use this package to provide a common user
;; interface, freeing them to concentrate on finding the completions
;; in the first place. The Emacs user can then customise the interface
;; once-and-for-all to suit their tastes, rather than separately
;; customising each package.
;;
;; Various completion mechanisms are provided, all of which can be
;; individually enabled, disabled and extensively tweaked via
;; customization variables:
;;
;; * Dynamic completion: inserts the best completion candidate in the
;;   buffer, highlighting the completed portion.
;;
;; * Completion hotkeys: single-key selection of a completion.
;;
;; * Cycling: cycle through completion candidates.
;;
;; * Tab-completion: "traditional" expansion to longest common
;;   substring.
;;
;; * Help-echo: display a list of completion candidates in the
;;   echo-area.
;;
;; * Tooltip: display a list of completion candidates in a tool-tip
;;   located below the point.
;;
;; * Completion menu: allow completion candidates to be selected from
;;   a drop-down menu located below the point.
;;
;; This package uses part of the automatic overlays package,
;; auto-overlay-common.el.


;;; Change Log:
;;
;; Version 0.2.1
;; * added commentary
;; * prevented any attempt to display tooltips and menus when not running X
;;
;; Version 0.2
;; * bug fixes (thanks to Mark Zonzon for patch)
;; * added `completion-min-chars' and `completion-delay' options (thanks to
;;   Jin Tong for suggestions)
;; * renamed to `completion-ui.el'
;; 
;; Version 0.1
;; * initial release


;;; Code:

(require 'auto-overlay-common)
(require 'cl)
(provide 'completion-ui)



(defgroup completion-ui nil
  "Completion user interface."
  :group 'convenience)


(defcustom completion-use-dynamic t
  "Enable dynamic completion."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-use-echo t
  "Display completions in echo area."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-use-tooltip t
  "Display completions in a tooltip."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-use-hotkeys t
  "Enable completion hotkeys (single-key selection of completions)."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-use-menu t
  "Enable completion menu."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-delay nil
  "Number of seconds to wait before activating completion mechanisms."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (float :tag "On")))


(defcustom completion-min-chars nil
  "Minimum number of characters before completions are offered."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))


(defface completion-dynamic-face
  '((((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "orange1")))
  "Face used for provisional completions during dynamic completion."
  :group 'completion-ui)


(defcustom completion-tooltip-delay 3
  "Number of seconds to wait after activating completion
mechanisms before displaying completions in a tooltip."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (float :tag "On")))


(defcustom completion-tooltip-timeout 15
  "Number of seconds to display completions in a tooltip
\(not relevant if help-echo text is displayed in echo area\)."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
		 (integer :tag "On")))


(defcustom completion-hotkey-list
  '([?0] [?1] [?2] [?3] [?4] [?5] [?6] [?7] [?8] [?9])
  "List of keys (vectors) to use for selecting completions when
`completion-use-hotkeys' is enabled."
  :group 'completion-ui
  :type '(repeat (vector character)))


(defcustom completion-auto-show-menu nil
  "Display completion menu automatically."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-resolve-old-method 'leave
  "Determines what to do if there's already a completion in
progress elsewhere in the buffer:

  'leave:   leave the old completion pending
  'accept:  automatically accept the old completion
  'reject:  automatically reject the old completion
  'ask:     ask what to do with the old completion"
  :group 'completion-ui
  :type '(choice (const :tag "leave" leave)
		 (const :tag "accept" accept)
		 (const :tag "reject" reject)
		 (const :tag "ask" ask)))


;; (defcustom completion-tooltip-x-offset 4
;;   "Horizontal pixel offset for tooltip.
;; Unfortunately, needs to be set manually to get tooltip in correct
;; position."
;;   :group 'completion-ui
;;   :type 'integer)


;; (defcustom completion-tooltip-y-offset 63
;;   "Vertical pixel offset for tooltip.
;; Unfortunately, needs to be set manually to get tooltip in correct
;; position."
;;   :group 'completion-ui
;;   :type 'integer)


(defvar completion-menu nil
  "Completion menu. If set, it should be a menu keymap or a function.
The function is called with two arguments, prefix and
completions, and should return a menu keymap.")


(defvar completion-accept-functions nil
  "Hook run after a completion is accepted.

Completions are accepted by calling `completion-accept',
selecting one with a hotkey, or selecting one from a
menu. Functions are passed one argument: the complete string that
was accepted \(prefix and accepted completion combined\).")


(defvar completion-reject-functions nil
  "Hook run after a completion is rejected.
Functions are passed the complete string that was rejected
\(prefix and rejected completion combined\).")


(defvar completion-tab-complete-functions nil
  "Hook run after tab-completion.
Functions are passed two arguments: the complete string that has
been inserted so far \(prefix and tab-completion combined\).")


(defvar completion-map nil
  "Keymap active when there's a completion at point.")

(defvar completion-activate-keymap t
  "When non-nil, activates `completion-map' keymap.")


(defvar completion-hotkey-map nil
  "Keymap used for hotkey completion (single-key selection of
  completions).

  Do NOT bind keys in this keymap directly. The keymap is
  constructed automatically from `completion-hotkey-list'. You
  should modify that instead, before `completion-ui.el' is
  loaded.")


(defvar completion-dynamic-map nil
  "Keymap active in a dynamic completion overlay.")


(defvar completion-menu-map nil
  "Keymap active when `completion-use-menu' is enabled.")





;;; =================================================================
;;;                   Setup default keymap bindings

;; set default bindings for the keymap assigned to completion overlays
(unless completion-dynamic-map
  (let ((map (make-sparse-keymap)))
    ;; <tab> does traditional tab-completion
    (define-key map "\t" 'completion-tab-complete)
    ;; M-<tab> cycles
    (define-key map [?\M-\t] 'completion-cycle)
    ;; M-<shift>-<tab> cycles backwards (note: [\M-\S-iso-lefttab] also works)
    (define-key map '[(meta shift iso-lefttab)]
      (lambda () "Cycle backwards through completions."
	(interactive) (completion-cycle -1)))
    ;; M-<space> abandons
    (define-key map "\M- " 'completion-reject)
;;    ;; <down> displays the completion menu
;;    (define-key map [down] 'completion-show-menu)
    ;; clicking on completion opens completion menu
    (define-key map [mouse-2] 'completion-show-menu)
    (setq completion-dynamic-map map)))



;; construct the keymap used for hotkey selection from completion-hotkey-list
(let ((map (make-sparse-keymap)) key)
  (dolist (key completion-hotkey-list)
    (define-key map key
      (lambda ()
	"Select a completion to insert if there is one, otherwise
run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay 'completion-select
					  'completion-use-hotkeys))))
  (setq completion-hotkey-map map))

;; make sure completion-hotkey-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-use-hotkeys minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-hotkey-map)
    (push (cons 'completion-use-hotkeys completion-hotkey-map)
	  minor-mode-map-alist)))



;; set default bindings for the keymap used when completion menu is enabled
(unless completion-menu-map
  (let ((map (make-sparse-keymap)))
    ;; M-<down> displays the completion menu
    (define-key map [M-down]
      (lambda ()
	"Display completion menu for current completion if there is one,
otherwise run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay 'completion-show-menu
					  'completion-use-menu)))
    (setq completion-menu-map map)))


;; make sure completion-menu-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-use-menu minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-menu-map)
    (push (cons 'completion-use-menu completion-menu-map)
	  minor-mode-map-alist)))




;; set default bindings for the main keymap
(unless completion-map
  (let ((map (make-sparse-keymap)))
    ;; <tab> does traditional tab-completion
    (define-key map "\t"
      (lambda ()
	"Tab-complete current completion if there is one, otherwise
run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay 'completion-tab-complete
					  'completion-activate-keymap)))
    ;; M-<tab> cycles
    (define-key map [?\M-\t]
      (lambda ()
	"Cycle through available completions if there are any, otherwise
run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay 'completion-cycle
					  'completion-activate-keymap)))
    ;; M-<shift>-<tab> cycles backwards
    (define-key map '[(meta shift iso-lefttab)]
      (lambda ()
	"Cycle backwards through completions if there are any, otherwise
run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay
	 (lambda () "Cycle backwards through completions."
	   (interactive) (completion-cycle -1))
	 'completion-activate-keymap)))
    
    ;; M-<space> abandons
    (define-key map "\M- "
      (lambda ()
	"Abandon current completion if there is one, otherwise
run whatever command would normally be bound to the key sequence."
	(interactive)
	(completion-run-if-within-overlay 'completion-reject
					  'completion-activate-keymap)))
    (setq completion-map map)))


;; make sure completion-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-activate-keymap minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-map)
    (push (cons 'completion-activate-keymap completion-map)
	  minor-mode-map-alist)))





;;; ============================================================
;;;                     Internal variables

(defvar completion-overlay-list nil
  "List of overlays used during completion")
(make-variable-buffer-local 'completion-overlay-list)


(defvar completion-tooltip-timer (timer-create)
  "Timer used to postpone tooltip until there's a pause in typing.")




;;; ===============================================================
;;;                  Public completion functions

(defun complete (prefix completions &optional overlay position)
  "Complete whatever's at point.

COMPLETIONS should be a list of possible completions,
i.e. strings which do not not include the PREFIX that's being
completed. If OVERLAY is supplied, use that instead of finding or
creating one.

If POSITION is supplied, activate completion mechanisms
immediately, irrespective of the setting of `completion-delay',
if point is at POSITION."

  ;; set overlay properties, getting completion overlay at point or creating a
  ;; new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions nil overlay))
  ;; get rid of any existing tooltip
  (tooltip-hide)
  ;; cancel any timer so that we don't have two running at once
  (cancel-timer completion-tooltip-timer)
  
  
  ;; only activate completion mechanisms if prefix has requisite number of
  ;; characters
  (unless (and completion-min-chars
	       (< (length prefix) completion-min-chars))

    ;; delay activating completion mechanisms if `completion-delay' is set
    (cond
     ((and completion-delay (null position))
      (setq completion-tooltip-timer
	    (run-with-idle-timer
	     completion-delay nil
	     'complete prefix completions overlay (point))))

     ;; if POSITION was supplied, only activate completion mechanisms if point
     ;; is there
     ((or (null position) (= position (point)))
      ;; activate dynamic completion
      (when completion-use-dynamic
	(complete-dynamic prefix completions overlay))
      
      ;; display completion echo text
      (when completion-use-echo
	(complete-echo prefix completions overlay))
      
      ;; display completion tooltip
      (when completion-use-tooltip
	(complete-tooltip prefix completions overlay))
      
      ;; activate completion menu
      (when completion-use-menu
	(complete-menu prefix completions overlay))
      
      ;; no need to run `complete-hotkeys' since all it does is create an
      ;; overlay, which we do anyway above
      )))
)



(defun complete-dynamic (prefix completions &optional overlay)
  "Insert dynamic completion and update completion overlay accordingly.
If OVERLAY is supplied, use that instead of finding or creating one."

  ;; set overlay properties, getting completion overlay at point or creating
  ;; a new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions 0 overlay))
  
  ;; resolve any old completions, keeping current overlay
  (completion-resolve-old overlay)
  
  ;; delete old completion and insert new one, if any
  (delete-region (overlay-start overlay) (overlay-end overlay))
  (when completions
    (insert (car completions))
    (move-overlay overlay (overlay-start overlay)
		  (+ (overlay-start overlay) (length (car completions)))))
  (goto-char (overlay-start overlay))
)



(defun complete-echo (prefix completions &optional overlay)
  "Display list of completions in echo area.
If OVERLAY is supplied, use that instead of finding or creating one."

  ;; set overlay properties, getting completion overlay at point or creating a
  ;; new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions t overlay))
  ;; display completions in echo area
  (message (completion-construct-echo-text prefix completions))
)



(defun complete-tooltip (prefix completions &optional overlay nodelay)
  "Display list of completions in a tooltip.
If OVERLAY is supplied, use that instead of finding or creating one. If
NODELAY is non-nil, display tooltip immediately, irrespective of the setting
of `completion-tooltip-delay'."

  ;; set overlay properties, getting completion overlay at point or creating a
  ;; new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions t overlay))
  
  ;; resolve any old completions, preserving current overlay
  (completion-resolve-old overlay)
  
  ;; cancel any running timer so we don't have two running at the same time
  (cancel-timer completion-tooltip-timer)
  
  ;; if `completion-tooltip-delay' is unset or NODELAY supplied, display
  ;; tooltip immediately
  (if (or (null completion-tooltip-delay) nodelay)
      (completion-show-tooltip overlay)
    ;; otherwise, postpone displaying tooltip until there's a pause in typing
    (setq completion-tooltip-timer
	  (run-with-idle-timer completion-tooltip-delay nil
			       'completion-show-tooltip
			       overlay (point))))
)



(defun complete-menu (prefix completions &optional overlay)
  "Enable completions menu for completion at point.
If OVERLAY is supplied, use that instead of finding or creating one."

  ;; set overlay properties, getting completion overlay at point or creating a
  ;; new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions t overlay))
  
  ;; when menu should be shown automatically...
  (when completion-auto-show-menu (completion-show-menu))
)



(defun complete-hotkeys (prefix completions &optional overlay)
  "Activate completion hotkeys (single-key selection of completions).
If OVERLAY is supplied, use that instead of finding or creating one."

  ;; set overlay properties, getting completion overlay at point or creating a
  ;; new one if we haven't been passed one
  (setq overlay (completion-setup-overlay prefix completions t overlay))
  
  ;; that's it! The rest of the work is done when `completion-select'
  ;; is  called
)





;;; ================================================================
;;;                    Public utility functions


(defun completion-show-menu (&optional overlay menu)
  "Show completion menu for completion at point.
If OVERLAY is supplied, use that instead of finding or creating
one.  If MENU is supplied, use that to construct the menu, unless
an overlay overrides it. Defaults to the \"overlay local\"
binding of 'completion-menu, or `completion-menu' if there is
none."
  (interactive)

  ;; menus only word under X windows at the moment
  (when (string= window-system "x")
    (when (null menu)
      (setq menu (auto-overlay-local-binding 'completion-menu)))
    ;; if we haven't been passed one, get completion overlay at point or create
    ;; new one if none exists
    (unless overlay (setq overlay (completion-overlay-at-point)))
    
    (let (keymap result)
      (cond
       ;; if `menu' is a function, evaluate it to get menu
       ((functionp menu)
	(setq keymap (funcall menu (overlay-get overlay 'prefix)
			      (overlay-get overlay 'completions)))
	;; throw error if return value has wrong type
	(unless (or (null keymap) (keymapp keymap))
	  (error "`completion-menu' returned wrong type: null or keymapp, %s"
		 (prin1-to-string keymap))))
       
       ;; if `menu' is a keymap, use that
       ((keymapp menu) (setq keymap menu))
       
       ;; if `menu' is null, evaluate `completion-construct-menu'
       ((null menu)
	(setq keymap (completion-construct-menu
		      (overlay-get overlay 'prefix)
		      (overlay-get overlay 'completions))))
       
       ;; otherwise, throw an error
       (t (error "Wrong type in `completion-menu': functionp or keymapp, %s"
		 (prin1-to-string menu))))
      
      
      ;; if we've constructed a menu, display it
      (when keymap
	(tooltip-hide)
	(setq result
	      (x-popup-menu (completion-posn-at-point-as-event
			     nil nil nil (+ (frame-char-height) 3))
			    keymap))
	
	
	;; if they ain't selected nuffin', don't do nuffin'!
	(when result
	  ;; convert result to a vector for key lookup
	  (setq result (apply 'vector result))
	  
	  (cond
	   ;; if they selected a completion from the menu...
	   ((string-match "^completion-insert"
			  (symbol-name (aref result (1- (length result)))))
	    ;; run accept hooks
	    (run-hook-with-args
	     'completion-accept-functions
	     (concat (overlay-get overlay 'prefix)
		     (buffer-substring (overlay-start overlay)
				       (overlay-end overlay))))
	    ;; insert selected completion
	    (setq completion-overlay-list
		  (delq overlay completion-overlay-list))
	    (delete-region (overlay-start overlay) (overlay-end overlay))
	    (delete-overlay overlay)
	    (funcall (lookup-key keymap result))
	    (tooltip-hide))
	   
	   ;; otherwise, run whatever they did select
	   (t (funcall (lookup-key keymap result))))
	  ))))
)




(defun completion-show-tooltip (&optional overlay position)
  "Show completion tooltip.

If OVERLAY is supplied, use that instead of finding one at point. If POSITION
is supplied, a tooltip will only be displayed if point is at position."
  (interactive)

  (when (and (string= window-system "x")
	     (or (null position) (= (point) position)))
    (unless overlay (setq overlay (completion-overlay-at-point)))
    
    ;; if point is in a completion overlay...
    (when overlay
      ;; note: there's no reliable way to calculate the *screen* position
      ;; (which is what x-show-tip requires) of point, so we use the kludge of
      ;; moving mouse to point, displaying mouse tooltip, and moving mouse
      ;; back
      (let ((restore (mouse-pixel-position))
	    (pos (completion-frame-posn-at-point))
	    params
	    (fg (face-attribute 'menu :foreground))
	    (bg (face-attribute 'menu :background))
	    (text (completion-construct-tooltip-text
		   (overlay-get overlay 'prefix)
		   (overlay-get overlay 'completions)
		   (overlay-get overlay 'completion-num))))
	
	;; use menu face and frame parameters
	;; FIXME: should we define our own?
	(when (stringp fg)
	  (setq params (tooltip-set-param params 'foreground-color fg))
	  (setq params (tooltip-set-param params 'border-color fg)))
	(when (stringp bg)
	  (setq params (tooltip-set-param params 'background-color bg)))
	(setq params (tooltip-set-param params 'internal-border-width 0))
	(setq params (tooltip-set-param params 'border-width 0))
;; 	(setq params (tooltip-set-param
;; 		      params 'left (+ (car pos) completion-tooltip-x-offset)))
;; 	(setq params (tooltip-set-param
;; 		      params 'top (+ (cdr pos) completion-tooltip-y-offset)))
	
	;; show tooltip
	;; note: we subtract a bit from x and y position so that mouse isn't
	;; on top of overlay when tooltip is displayed, otherwise overlay's
	;; help-echo tooltip appears, removing our tooltip, and it too
	;; disappears when mouse position is set back (so nothing gets
	;; displayed)
 	(set-mouse-pixel-position (selected-frame)
 				  (- (car pos) 1) (cdr pos))
	(x-show-tip text nil params completion-tooltip-timeout
		    0 (frame-char-height))
 	(set-mouse-pixel-position (car restore) (cadr restore) (cddr restore))
	)))
)




(defun completion-accept (&optional overlay)
  "Accept current dynamic completion.
If OVERLAY is supplied, use that instead of finding or creating one."
  (interactive)

  ;; if we haven't been passed one, get completion overlay at point or create
  ;; new one if none exists
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if point is in a completion overlay...
  (when overlay
    ;; accept current completion
    (goto-char (overlay-end overlay))
    (setq completion-overlay-list (delq overlay completion-overlay-list))
    ;; run accept hooks
    (run-hook-with-args 'completion-accept-functions
			(concat (overlay-get overlay 'prefix)
				(buffer-substring (overlay-start overlay)
						  (overlay-end overlay))))
    (delete-overlay overlay)
    (tooltip-hide))
)



(defun completion-reject (&optional overlay)
  "Reject current dynamic completion.
If OVERLAY is supplied, use that instead of finding or creating one."
  (interactive)

  ;; if we haven't been passed one, get completion overlay at point or create
  ;; new one if none exists
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if point is in a dynamic completion overlay...
  (when overlay
    ;; reject current completion
    (setq completion-overlay-list (delq overlay completion-overlay-list))
    (delete-region (overlay-start overlay) (overlay-end overlay))
    ;; run reject hooks
    (run-hook-with-args 'completion-reject-functions
			(concat (overlay-get overlay 'prefix)
				(buffer-substring (overlay-start overlay)
						  (overlay-end overlay))))
    (delete-overlay overlay)
    (tooltip-hide))
)



(defun completion-cycle (&optional n overlay)
  "Cycle through available completions.

Optional argument N specifies the number of completions to cycle
forwards \(backwards if negative\). Default is 1. Interactively,
N is the prefix argument.

If OVERLAY is supplied, use that instead of finding or creating one."
  (interactive "P")
  (when (null n) (setq n 1))
  
  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if within a completion overlay, cycle to next completion
  (when overlay
    (let* (i string)
      (when (null (setq i (overlay-get overlay 'completion-num)))
	(setq i -1))
      (setq i (mod (+ i n) (length (overlay-get overlay 'completions))))
      (setq string (nth i (overlay-get overlay 'completions)))
      ;; delete old completion and insert new one
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert string)
      (move-overlay overlay (overlay-start overlay)
		    (+ (overlay-start overlay) (length string)))
      (overlay-put overlay 'completion-num i)
      (goto-char (overlay-start overlay))
      (tooltip-hide)
      ;; display echo text and tooltip if using them
      (when completion-use-echo
	(complete-echo (overlay-get overlay 'prefix)
		       (overlay-get overlay 'completions)
		       overlay))
      (when completion-use-tooltip
	(complete-tooltip (overlay-get overlay 'prefix)
			  (overlay-get overlay 'completions)
			  overlay 'no-delay))))
)



(defun completion-select (&optional n overlay)
  "Select completion corresponding to the last input event
when hotkey completion is active.

If integer N is supplied, insert completion corresponding to that
instead. If OVERLAY is supplied, use that instead of finding or
creating one.

Intended to be bound to keys in `completion-hotkey-map'."
  (interactive)
  
  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; find completion index corresponding to last input event
  (unless n
    (setq n (position (this-command-keys-vector) completion-hotkey-list
		      :test 'equal)))
  
  ;; if within a completion overlay...
  (when overlay
    (let ((completions (overlay-get overlay 'completions)))
      (cond
       ;; if there are no completions, run whatever would otherwise be bound to
       ;; the key
       ((null completions)
	(when (and (boundp 'trap-recursion) trap-recursion)
	  (error "Recursive call to `completion-select'"))
	(setq completion-use-hotkeys nil)
	(let ((trap-recursion t))
	  (unwind-protect
	      (command-execute
	       (key-binding (this-command-keys) 'accept-default))
	    (setq completion-use-hotkeys t))))
       
       ;; if there are too few completions, display message
       ((>= n (length completions))
	(beep)
	(message "Only %d completions available"
		 (length (overlay-get overlay 'completions))))
       
       ;; otherwise, replace dynamic completion with selected one
       (t
	(setq completion-overlay-list (delq overlay completion-overlay-list))
	(delete-region (overlay-start overlay) (overlay-end overlay))
	(insert (nth n completions))
	;; run accept hooks
	(run-hook-with-args 'completion-accept-functions
			    (concat (overlay-get overlay 'prefix)
				    (nth n completions)))
	(delete-overlay overlay)
	(when completion-use-tooltip (tooltip-hide)))
       )))
)



(defun completion-tab-complete (&optional overlay)
  "Tab-complete completion at point
\(i.e. insert longest common prefix of all the completions\).

If OVERLAY is supplied, use that instead of finding or creating one."
  (interactive)
  
  (unless overlay (setq overlay (completion-overlay-at-point)))
  
  ;; if within a completion overlay
  (when overlay
    (let ((str (try-completion "" (overlay-get overlay 'completions))))
      (unless (or (null str) (string= str ""))
	;; do tab-completion
	(delete-region (overlay-start overlay) (overlay-end overlay))
	(insert str)
	(move-overlay overlay (point) (point))
	(overlay-put overlay 'prefix
		     (concat (overlay-get overlay 'prefix) str))
	(overlay-put overlay 'completions nil))
      ;; run tab-complete hooks
      (run-hook-with-args 'completion-tab-complete-functions
			  (overlay-get overlay 'prefix))))
)





;;; ==============================================================
;;;                    Internal functions

(defun completion-resolve-old (&optional overlay)
  "Resolve old dynamic completions according to the setting of
`completion-reslove-method'. Any completion overlay specified by
OVERLAY will be left alone."

  ;; temporarily remove ignored overlay from list
  (setq completion-overlay-list (delq overlay completion-overlay-list))
  
  (cond
   ;; leave old completions (but accept zero-length ones)
   ((eq completion-resolve-old-method 'leave)
    (mapc (lambda (o)
	    (overlay-put o 'evaporate t)
	    (unless (overlay-buffer o)
	      (setq completion-overlay-list
		    (delq o completion-overlay-list))
	      	    (run-hook-with-args 'completion-accept-functions
					(overlay-get o 'prefix))))
	  completion-overlay-list))
   
   ;; accept old completions
   ((eq completion-resolve-old-method 'accept)
    (mapc (lambda (o)
	    (run-hook-with-args 'completion-accept-functions
				(concat (overlay-get overlay 'prefix)
					(buffer-substring
					 (overlay-start o)
					 (overlay-end o))))
	    (delete-overlay o))
	  completion-overlay-list)
    (setq completion-overlay-list nil))
   
   ;; reject old completions
   ((eq completion-resolve-old-method 'reject)
    (mapc (lambda (o)
	    (run-hook-with-args 'completion-reject-functions
				(concat (overlay-get overlay 'prefix)
					(buffer-substring
					 (overlay-start o)
					 (overlay-end o))))
	    (delete-region (overlay-start o) (overlay-end o))
	    (delete-overlay o))
	  completion-overlay-list)
    (setq completion-overlay-list nil))
   
   ;; ask 'em
   ((eq completion-resolve-old-method 'ask)
    (save-excursion
      (mapc (lambda (o)
	      (goto-char (overlay-end o))
	      ;; FIXME: remove hard-coded face
	      (overlay-put o 'face '(background-color . "red"))
	      (if (y-or-n-p "Accept completion? ")
		  ;; accept
		  (run-hook-with-args
		   'completion-accept-functions
		   (concat (overlay-get overlay 'prefix)
			   (buffer-substring (overlay-start o)
					     (overlay-end o))))
		;; reject
		(run-hook-with-args
		 'completion-reject-functions
		 (concat (overlay-get overlay 'prefix)
			 (buffer-substring (overlay-start o)
					   (overlay-end o))))
		(delete-region (overlay-start o) (overlay-end o)))
	      (delete-overlay o))
	    completion-overlay-list)
      (setq completion-overlay-list nil))))

  
  ;; add ignored overlay back into the list
  (when (overlayp overlay) (push overlay completion-overlay-list))
)



(defun completion-run-if-within-overlay (command variable)
  "Run COMMAND if within a completion overlay,
otherwise run whatever command would normally be bound to the key
sequence used to call this function.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset to t
after at the end of this function.

Intended to be bound to a key sequence in a keymap."
  (interactive)

  ;; throw and error if executing recursively
  (when (and (boundp 'trap-recursion) trap-recursion)
    (error "Recursive call to `completion-run-if-within-overlay'; supplied\
 variable probably doesn't disable keymap"))
  
  ;; if within an overlay, run command
  (if (completion-overlay-at-point)
      (command-execute command)
    
    ;; otherwise, run whatever would normally be bound to the key sequence
    (set variable nil)
    (let ((trap-recursion t))
      (unwind-protect
	  (command-execute (key-binding (this-command-keys) 'accept-default))
	(set variable t))))
)



(defun completion-construct-tooltip-text (prefix completions &optional num)
  "Function to return completion text for a tooltip.
Optional argument NUM specifies the number of the currently inserted dynamic
completion."
  
  (let* ((text "") str
	 (maxlen (apply 'max (mapcar 'length completions))))
    
    (dotimes (i (length completions))
      ;; pad all strings to same length
      (setq str (concat prefix (nth i completions)
			(make-string
			 (- maxlen (length (nth i completions))) ? )))
      ;; if using hotkeys and one is assigned to current completion, show
      ;; it next to completion text
      (when (and completion-use-hotkeys
		 (< i (length completion-hotkey-list)))
	(setq str
	      (concat str " "
		      (format "(%s)" (key-description
				      (nth i completion-hotkey-list))))))
      ;; if current completion is the inserted dynamic completion, use
      ;; `completion-dynamic-face' to highlight it
      (if (and num (= i num))
	  ;; setting 'face attribute to 'completion-dynamic-face doesn't seem
	  ;; to work with defface using display classes
	  (put-text-property 0 (length str) 'face
			     (cons 'background-color
				   (face-attribute 'completion-dynamic-face
						   :background))
			     str)
	(put-text-property 0 (length str) 'face 'menu str))
      (setq text (concat text str "\n")))
      
    ;; return constructed text
    text)
)



(defun completion-construct-echo-text (prefix completions)
  "Function to return completion text for echo area."
  
  (let* ((text "") str)
    (dotimes (i (length completions))
      (setq str (concat prefix (nth i completions)))
      ;; if using hotkeys and one is assigned to current completion, show it
      ;; next to completion text
      (cond
       ((and completion-use-hotkeys (< i (length completion-hotkey-list)))
	(setq str
	      (concat
	       (format "(%s) "
		       (key-description
			(nth i completion-hotkey-list))) str)))
       (completion-use-hotkeys
	(setq str (concat "() " str))))
      (setq text (concat text str "  ")))
    
    ;; return constructed text
    text)
)



(defun completion-construct-help-echo-text (dummy1 overlay dummy2)
  "Function to return text for help-echo property of completion overlay."
  
  (let* ((text "") str
	 (prefix (overlay-get overlay 'prefix))
	 (completions (overlay-get overlay 'completions))
	 (num (overlay-get overlay 'completion-num)))

    ;; if `tooltip-mode' is enabled, construct text for tooltip
    (if tooltip-mode
	(dotimes (i (length completions))
	  ;; if using hotkeys and one is assigned to current completion, show
	  ;; it next to completion text
	  (if (and completion-use-hotkeys
		   (< i (length completion-hotkey-list)))
	      (setq str
		    (format "(%c)"
			    (key-description (nth i completion-hotkey-list))))
	    (setq str "    "))
	  ;; add completion to text
	  (setq str (concat str " " prefix (nth i completions)))
	  (setq text (concat text str "\n")))

      ;; otherwise, construct text for echo area
      (setq text (completion-construct-echo-text prefix completions)))
    
    ;; return constructed text
    text)
)




(defun completion-construct-menu (prefix completions)
  "Construct and return menu keymap defining the completion menu."

  (let ((menu (make-sparse-keymap))
	(num (length completions))
	n)
    
    ;; construct menu keymap from available completions
    (dotimes (i num)
      (setq n (- num i 1))
      (define-key menu
;;	[completion-insert]
	(vector (intern (concat "completion-insert-" (number-to-string n))))
	(list 'menu-item
	      (concat prefix (nth n completions))
	      `(lambda () (insert ,(nth n completions)))
	      ;; if a hotkeys is associated with completion, show it in menu
	      :keys (when (and completion-use-hotkeys
			       (< n (length completion-hotkey-list)))
		      (key-description (nth n completion-hotkey-list))))))
    
    ;; return the menu keymap
    menu)
)



(defun completion-posn-at-point-as-event (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))
  
  (let* ((pos (posn-at-point position window))
	 (x-y (posn-x-y pos))
	 (edges (window-inside-pixel-edges window))
	 (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
	    (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
		  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos))
)



(defun completion-setup-overlay (prefix completions num overlay)
  "Get completion overlay at point, or create a new one if none exists,
and set its properties according to PREFIX, COMPLETIONS and NUM. If NUM is t,
the overlay's completion-num property is left unchanged."

  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; if overlay does not already exists, create one
  (unless overlay
    (setq overlay (make-overlay (point) (point)))
    ;; set permanent overlay properties
    (overlay-put overlay 'completion-overlay t)
    (overlay-put overlay 'face 'completion-dynamic-face)
    (overlay-put overlay 'keymap completion-dynamic-map)
    (overlay-put overlay 'help-echo 'completion-construct-help-echo-text)
    (overlay-put overlay 'priority 100)
    ;; add overlay to list
    (push overlay completion-overlay-list))
  
  ;; update modifiable overlay properties
  (overlay-put overlay 'prefix prefix)
  (overlay-put overlay 'completions completions)
  (unless (eq num t) (overlay-put overlay 'completion-num num))
  
  ;; return the new overlay
  overlay
)



(defun completion-overlay-at-point (&optional point)
  "Return dynamic completion overlay overlapping point.
\(There should only be one; if not, one is returned at random\)"
  (when (null point) (setq point (point)))
  
  (let (overlay
	(modified (buffer-modified-p))
	(inhibit inhibit-modification-hooks)
	(undo buffer-undo-list))
    
    (save-excursion
      ;; there's no inbuilt function that finds all overlays overlapping point
      ;; including all zero-length overlays, so we use the ugly kludge of
      ;; inserting a character then deleting it, necessitating inhibiting
      ;; modification hooks and saving/restoring the buffer's modified flag
      (setq inhibit-modification-hooks t)
      (goto-char point)
      (insert " ")

      ;; find completion overlays
      (catch 'found
	(dolist (o (overlays-in (1- (point)) (point)))
	  (when (overlay-get o 'completion-overlay)
	    (setq overlay o)
	    (throw 'found t))))
      
      ;; restore buffer properties
      (delete-backward-char 1)
      (setq inhibit-modification-hooks inhibit)
      (set-buffer-modified-p modified)
      (setq buffer-undo-list undo))

    ;; return the overlay we've found
    overlay)
)



(defun completion-window-posn-at-point (&optional position window)
  "Return pixel position of top left of corner glyph at POSITION,
relative to top left corner of WINDOW. Defaults to the position of point in
the selected window.

See also `completion-window-inside-posn-at-point' and
`completion-frame-posn-at-point'."
  
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  
  (let ((x-y (posn-x-y (posn-at-point position window)))
	(edges (window-inside-pixel-edges window))
	(win-x-y (window-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (- (car win-x-y)))
	  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)))))
)



(defun completion-window-inside-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of the text area in WINDOW. Defaults to the
position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-frame-posn-at-point'.."
  
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (posn-x-y (posn-at-point position window))
)



(defun completion-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults to the
position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-window-inside-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  
  (let ((x-y (posn-x-y (posn-at-point position window)))
	(edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
	  (+ (cdr x-y) (cadr edges))))
)





;;; ===============================================================
;;;                       Compatibility Stuff

(unless (fboundp 'posn-at-point)
  (require 'predictive-compat)
  (defalias 'completion-posn-at-point-as-event
            'predictive-compat-posn-at-point-as-event)
  (defalias 'completion-frame-posn-at-point
            'predictive-compat-frame-posn-at-point)
)


;;; predictive-completion.el ends here
