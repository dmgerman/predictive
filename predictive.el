
;;; predictive.el --- predictive completion minor mode for Emacs

;; Copyright (C) 2004 2005 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.7
;; Keywords: predictive, completion

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
;; Version 0.7
;; * switch-dictionary code moved to separate, more general, more efficient,
;;   completely re-written, and stand-alone `auto-overlays' package
;;
;; Version 0.6.1
;; * minor bug fixes
;;
;; Version 0.6
;; * predictive-auto-add-to-dict no longer buffer-local
;; * minor bug fixes
;; (major version bump because dict.el provides new `dict-dump-words-to-file'
;; function)
;;
;; Version 0.5.1
;; * fixed bugs in 'word, 'start and 'end regexp parsing code
;; * fixed bug in predictive-overlay-suicide
;;
;; Version 0.5
;; * added auto-learn and auto-add caching
;; * overhauled switch-dictionary code (again!), changed the way 'word regexps
;;   work and added new 'line regexps
;;
;; Version 0.4
;; * tidied and fixed bugs in switch-dictionary code
;; * added option to display active dictionary in mode line
;; * added functions to learn from buffers and files
;; * cleaned up auto-learning and auto-adding code
;;
;; Version 0.3.1
;; * fixed bugs in switch-dictionary regions
;;
;; Version 0.3
;; * added significantly more powerful dictionary switching features
;; * removed redundant predictive-scoot-and-insert function (same effect can
;;   easily be achieved with predictive-scoot-ahead and predictive-accept)
;;
;; Version 0.2.1:
;; * repackaging
;; * removed c setup function (should be provided in separate package)
;;
;; Version 0.2:
;; * added options for dictionary autosaving
;; * other changes required for compatibility with dict.el version 0.2
;; * fixed auto-learn bugs
;; * explicitly require cl.el
;;
;; Version 0.1:
;; * initial release



;;; Code:

(provide 'predictive)
(require 'dict)
(require 'auto-overlays)
(require 'easy-mmode)

;; the only required common-lisp function is `position', so this dependency
;; should really be removed
(require 'cl)




;;; ================================================================
;;;          Customization variables controling predictive mode 

(defgroup predictive nil
  "Predictive completion."
  :group 'convenience)


(defcustom predictive-dynamic-completion t
  "*In predictive mode, enables the dynamic completion facility.
When non-nil, the most likely ompletion is provisionally inserted after every
character is typed. It can be accepted using `predictive-accept-completion' or
`predictive-accept-and-insert'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-offer-completions t
  "*In predictive mode, enables the offer-completions facility.
When non-nil, predictive mode offers a list of possible completions which can
be inserted by typing the appropriate number"
  :group 'predictive
  :type 'boolean)


(defcustom predictive-accept-on-select t
  "*In predictive mode, controls the behaviour when a completion is selected
from the list offered when `predictive-offer-completions' is enabled. When
non-nil, the selected completion is inserted and accepted. When nil, the
selected completion is inserted, and the resulting string is completed anew."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-always-complete nil
  "*In predictive mode, try to complete words when nothing has been typed yet!
This has the effect of making the most likely words in whole the dictionary
available. How to insert these words depends on the settings of
`predictive-dynamic-completion' and `predictive-offer-completions'.

Warning: could drive you mad! Disabled by default to protect your sanity."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-accept-if-point-moved t
  "*In predictive mode, how to resolve a completion if point has moved
away from it. If non-nil, the completion is accepted. If nil, it is abandoned
instead."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-max-completions 10
  "*Maximum number of completions to return in predictive mode."
  :group 'predictive
  :type 'integer)


(defcustom predictive-completion-speed 0.1
  "*In predictive mode, sets the default completion speed for new dictionaries.
The completion speed is a desired upper limit on the time it takes to find
completions, in seconds. However, there is no guarantee it will be achieved!
Lower values result in faster completion, at the expense of dictionaries
taking up more memory."
  :group 'predictive
  :type 'number)


(defcustom predictive-dict-autosave t
  "*In predictive mode, sets the default autosave flag for new dictionaries.
A value of t means modified dictionaries will be saved automatically when
unloaded. The symbol 'ask' means you will be prompted to save modified
dictionaries. A value of nil means dictionaries will not be saved
automatically, and unless you save the dictionary manually all changes will be
lost when the dictionary is unloaded. See also `dict-save'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-ignore-initial-caps t
  "*In predictive mode, controls whether to ignore initial capital letters
when searching for completions. If non-nil, completions for the uncapitalized
string are also found.

Note that only the *first* capital letter of a string is ignored. Thus typing
\"A\" would find \"and\", \"Alaska\" and \"ANSI\", but typing \"AN\" would
only find \"ANSI\", whilst typing \"a\" would only find \"and\"."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-auto-learn nil
  "*In predictive mode, controls automatic word frequency learning.
When non-nil, the frequency count for that word is incremented each time a
completion is accepted, making the word more likely to be offered higher up
the list of completions in the future."
  :group 'predictive
  :type 'boolean)


;; this variable should be set in a setup function, so not a defcustom
(defvar predictive-auto-add-to-dict nil
  "*In predictive mode, controls automatic adding of new words to dictionaries.
If nil, words are never automatically added to a dictionary. If t, new words
\(i.e. words that are not in the dictionary\) are automatically added to the
active dictionary.

If set to a dictionary name (a symbol), new words are automatically added to
that dictionary instead of the active one. If set to the special symbol
'buffer', new words are automatically added to a word list at the end of the
buffer. If `predctive-add-to-dict-ask' is enabled, predictive mode will ask
before adding any word.")
;;(make-variable-buffer-local 'predictive-auto-add-to-dict)


(defcustom predictive-add-to-dict-ask t
  "*If non-nil, predictive mode will ask before automatically adding a word
to a dictionary. Enabled by default. This has no effect unless
`predictive-auto-add-to-dict' is enabled."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-use-auto-learn-cache t
  "*If non-nil, predictive mode will cache auto-learned and auto-added words,
and only add them to the dictionary when Emacs is idle. This makes predictive
mode more responsive, since adding words can otherwise cause a small but
noticeable delay when typing.

See also `predictive-flush-auto-learn-delay'."
  :group 'predictive
  :type 'boolean)


(defcustom predictive-flush-auto-learn-delay 60.
  "*In predictive mode, time to wait before flushing auto-learn/add caches.
The caches will only be flushed after Emacs has been idle for this many
seconds. To take effect, this variable must be set before predictive mode is
enabled.

This has no effect unless `predictive-use-auto-learn-cache' is enabled."
  :group 'predictive
  :type 'number)


(defcustom predictive-which-dict nil
  "*If non-nil, the predictive mode dictionary is shown in the mode line."
  :group 'predictive
  :type 'boolean)




;; Default values for the following variables are set by code at the end of
;; this file or by setup functions

(defvar predictive-mode-hook nil
  "Hook run after predictive mode is enabled.")

(defvar predictive-mode-disable-hook nil
  "Hook run after predictive mode is disabled.")


(defvar predictive-main-dict nil
  "Main dictionary to use in a predictive mode buffer.
It should be the symbol of a loaded dictionary. It can also be a list of such
symbols, in which case predictive mode searches for completions in all of
them, as though they were one large dictionary.

Note that using lists of dictionaries can lead to unexpected effets when
auto-learn or auto-add-to-dict are used. If auto-learn is enabled, weights
will be updated in the first dictionary in the list that contains the word
being updated \(see `predictive-auto-learn'\). Similarly, if auto-add-to-dict
is set to t, words will be added to the first dictionary in the list \(see
`predictive-auto-add-to-dict'\).")


(defvar predictive-buffer-dict nil
  "Buffer-local dictionary used in a predictive mode buffer,
constructed from a word list the end of the buffer.

Note that when using auto-learn, the buffer dictionary has lowest priority:
weights will only be updated in the buffer dictionary if the word does not
exist in the active dictionary \(see `predictive-auto-learn'\). It is better
to ensure that the buffer word list does not duplicate words already in other
dictionaries.")
(make-variable-buffer-local 'predictive-buffer-dict)


(defvar predictive-syntax-alist nil
  "Alist associating character syntax descriptors with completion functions.
Used by the predictive mode `predictive-self-insert' function to decide what
to do based on a typed character's syntax.

By default, all printable characters are bound to `predictive-self-insert' in
predictive mode. Therefore, unless you know what you are doing, it is a wise
plan to ensure that all functions in the alist insert the last input event, in
addition to any completion-related action. All predictive mode functions that
do this have \"insert\" in their names.")


(defvar predictive-override-syntax-alist nil
  "Alist associating characters with completion functions in predictive mode.
Overrides the default function based on a typed character's syntax. Used by
`predictive-self-insert'.")


(defvar predictive-major-mode-alist nil
  "Alist associating a major mode symbol with a function in predictive mode.
The alist is checked whenever predictive mode is turned on in a buffer, and if
the buffer's major made matches one in the alist, the associated function is
called. This makes it easier to customize predictive mode for different major
modes.")


(defvar predictive-map nil
  "Keymap used in predictive mode.")


(defvar predictive-completing-map nil
  "Keymap used when in the process of completing a word in predictive mode.")


(defvar predictive-offer-completions-keylist nil
  "In predictive mode, list of characters to use for selecting completions
from the list offered when `predictive-offer-completions' is enabled. Default
is numerical characters 0 to 9.")


(defvar predictive-offer-completions-map nil
  "Keymap used when there are completions on offer in predictive mode
\(only used if `predictive-offer-completions' is enabled). Constructed from
`predictive-offer-completions-keylist'.

Setting this directly will have no effect. Instead, set
`predictive-offer-completions-keylist'")





;;; ================================================================
;;;             Internal variables to do with completion

;; Overlay used during completion.
(defvar predictive-overlay (make-overlay 0 0))
(make-variable-buffer-local 'predictive-overlay)
(delete-overlay predictive-overlay)  ; just removes overlay from buffer
(overlay-put predictive-overlay 'face 'highlight)
;; ;; add key-bindings for completion selection if offer-completions is on
;; (when predictive-offer-completions 
;;   (overlay-put predictive-overlay 'keymap 'predictive-offer-map))


;; Stores marker locating the last completion accepted or abandoned because the
;; point had moved.
(defvar predictive-old-completion-marker nil)
(make-variable-buffer-local 'predictive-old-completion-marker)


;; Non-nil when completions are on offer. Its effect is to enable the
;; `predictive-offer-completions-map' keymap.
(defvar predictive-completions-on-offer nil)
(make-variable-buffer-local 'predictive-completions-on-offer)


;; ;; Stores the currently active dictionary.
;; (defvar predictive-current-dict nil)
;; (make-variable-buffer-local 'predictive-current-dict)


;; Stores vector of current completions.
(defvar predictive-completions nil)
(make-variable-buffer-local 'predictive-completions)


;; Stores current state of predictive completion process.
(defvar predictive-typed-string nil)
(make-variable-buffer-local 'predictive-typed-string)

(defvar predictive-completion-num nil)
(make-variable-buffer-local 'predictive-completion-num)


;; hash tables used for auto-learn and auto-add caching
(defvar predictive-auto-learn-cache nil)
(make-variable-buffer-local 'predictive-auto-learn-cache)

(defvar predictive-auto-add-cache nil)
(make-variable-buffer-local 'predictive-auto-add-cache)

;; timer for flushing auto-learn and auto-add caches
(defvar predictive-flush-auto-learn-timer nil)



;;; ==============================================================
;;;          Internal variables to do with dictionaries

;; stores list of dictionaries used by buffer
(defvar predictive-used-dict-list nil)
(make-variable-buffer-local 'predictive-used-dict-list)





;;; ==============================================================
;;;                 Predictive mode keymap

;; Set the default keymap if it hasn't been defined already (most likely in an
;; init file or setup function)
(unless predictive-map
  (let ((map (make-keymap)))
    ;; All printable characters run predictive-insert, which decides what to do
    ;; based on the character's syntax
    (define-key map "A" 'predictive-self-insert)
    (define-key map "a" 'predictive-self-insert)
    (define-key map "B" 'predictive-self-insert)
    (define-key map "b" 'predictive-self-insert)
    (define-key map "C" 'predictive-self-insert)
    (define-key map "c" 'predictive-self-insert)
    (define-key map "D" 'predictive-self-insert)
    (define-key map "d" 'predictive-self-insert)
    (define-key map "E" 'predictive-self-insert)
    (define-key map "e" 'predictive-self-insert)
    (define-key map "F" 'predictive-self-insert)
    (define-key map "f" 'predictive-self-insert)
    (define-key map "G" 'predictive-self-insert)
    (define-key map "g" 'predictive-self-insert)
    (define-key map "H" 'predictive-self-insert)
    (define-key map "h" 'predictive-self-insert)
    (define-key map "I" 'predictive-self-insert)
    (define-key map "i" 'predictive-self-insert)
    (define-key map "J" 'predictive-self-insert)
    (define-key map "j" 'predictive-self-insert)
    (define-key map "K" 'predictive-self-insert)
    (define-key map "k" 'predictive-self-insert)
    (define-key map "L" 'predictive-self-insert)
    (define-key map "l" 'predictive-self-insert)
    (define-key map "M" 'predictive-self-insert)
    (define-key map "m" 'predictive-self-insert)
    (define-key map "N" 'predictive-self-insert)
    (define-key map "n" 'predictive-self-insert)
    (define-key map "O" 'predictive-self-insert)
    (define-key map "o" 'predictive-self-insert)
    (define-key map "P" 'predictive-self-insert)
    (define-key map "p" 'predictive-self-insert)
    (define-key map "Q" 'predictive-self-insert)
    (define-key map "q" 'predictive-self-insert)
    (define-key map "R" 'predictive-self-insert)
    (define-key map "r" 'predictive-self-insert)
    (define-key map "S" 'predictive-self-insert)
    (define-key map "s" 'predictive-self-insert)
    (define-key map "T" 'predictive-self-insert)
    (define-key map "t" 'predictive-self-insert)
    (define-key map "U" 'predictive-self-insert)
    (define-key map "u" 'predictive-self-insert)
    (define-key map "V" 'predictive-self-insert)
    (define-key map "v" 'predictive-self-insert)
    (define-key map "W" 'predictive-self-insert)
    (define-key map "w" 'predictive-self-insert)
    (define-key map "X" 'predictive-self-insert)
    (define-key map "x" 'predictive-self-insert)
    (define-key map "Y" 'predictive-self-insert)
    (define-key map "y" 'predictive-self-insert)
    (define-key map "Z" 'predictive-self-insert)
    (define-key map "z" 'predictive-self-insert)
    (define-key map "'" 'predictive-self-insert)
    (define-key map "-" 'predictive-self-insert)
    (define-key map "<" 'predictive-self-insert)
    (define-key map ">" 'predictive-self-insert)
    (define-key map " " 'predictive-self-insert)
    (define-key map "." 'predictive-self-insert)
    (define-key map "," 'predictive-self-insert)
    (define-key map ":" 'predictive-self-insert)
    (define-key map ";" 'predictive-self-insert)
    (define-key map "?" 'predictive-self-insert)
    (define-key map "!" 'predictive-self-insert)
    (define-key map "\"" 'predictive-self-insert)
    (define-key map "0" 'predictive-self-insert)
    (define-key map "1" 'predictive-self-insert)
    (define-key map "2" 'predictive-self-insert)
    (define-key map "3" 'predictive-self-insert)
    (define-key map "4" 'predictive-self-insert)
    (define-key map "5" 'predictive-self-insert)
    (define-key map "6" 'predictive-self-insert)
    (define-key map "7" 'predictive-self-insert)
    (define-key map "8" 'predictive-self-insert)
    (define-key map "9" 'predictive-self-insert)
    (define-key map "~" 'predictive-self-insert)
    (define-key map "`" 'predictive-self-insert)
    (define-key map "@" 'predictive-self-insert)
    (define-key map "#" 'predictive-self-insert)
    (define-key map "$" 'predictive-self-insert)
    (define-key map "%" 'predictive-self-insert)
    (define-key map "^" 'predictive-self-insert)
    (define-key map "&" 'predictive-self-insert)
    (define-key map "*" 'predictive-self-insert)
    (define-key map "_" 'predictive-self-insert)
    (define-key map "+" 'predictive-self-insert)
    (define-key map "=" 'predictive-self-insert)
    (define-key map "(" 'predictive-self-insert)
    (define-key map ")" 'predictive-self-insert)
    (define-key map "{" 'predictive-self-insert)
    (define-key map "}" 'predictive-self-insert)
    (define-key map "[" 'predictive-self-insert)
    (define-key map "]" 'predictive-self-insert)
    (define-key map "|" 'predictive-self-insert)
    (define-key map "\\" 'predictive-self-insert)
    (define-key map "/" 'predictive-self-insert)
    
    ;; DEL deletes backwards and removes characters from the current
    ;; completion, if any
    (define-key map "\d" 'predictive-backward-delete-and-complete)
    
    ;; M-<tab> completes word at or next to point
    (define-key map [?\M-\t] 'predictive-complete-word-at-point)
    
    ;; RET inserts a newline and updates switch-dictionary regions
    (define-key map "\r" 'predictive-accept-and-newline)
    
    (setq predictive-map map)
  )
)




;;; ===============================================================
;;;                   The minor mode definition

(define-minor-mode predictive-mode
  "Toggle predictive mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

Note that simply setting the minor-mode variable `predictive-mode' is
not sufficient to enable predictive mode. Use the command
`turn-on-predictive-mode' instead when adding to hooks.

Predictive mode implements predictive text completion, in an attempt to save
on typing. It looks up completions for the word currently being typed in a
dictionary. Completions can be inserted in a variety of ways, depending on how
intrusive you want it to be. See variables `predictive-dynamic-completion' and
`predictive-offer-completions', and the predictive mode documentation."
  
  ;; initial value
  nil
  ;; mode-line indicator
  (" Predict"
   (predictive-which-dict-mode ("[" predictive-which-dict-name "]")))
  ;; keymap
  predictive-map
  
  ;; if predictive mode has been disabled...
  (if (not predictive-mode)
      (progn
	;; turn off which-dict mode
	(predictive-which-dict-mode 0)
	;; cancel the auto-learn/cache timer and flush the caches
	(cancel-timer predictive-flush-auto-learn-timer)
	(predictive-flush-auto-learn-caches)
	(remove-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches t)
	;; clear the used-dictionary list
	(setq predictive-used-dict-list nil)
	;; run the hook
	(run-hooks 'predictive-mode-disable-hook))

    
    ;; if predictive  mode has been enabled...

    ;; create the buffer-local dictionary
    (predictive-create-buffer-dict)
    ;; create hash tables for auto-learn and auto-add caching
    (setq predictive-auto-learn-cache (make-hash-table :test 'equal))
    (setq predictive-auto-add-cache (make-hash-table :test 'equal))
    ;; make sure auto-learn/add caches are flushed if buffer is killed
    (add-hook 'kill-buffer-hook 'predictive-flush-auto-learn-caches nil t)
    
    ;; look up major mode in major-mode-alist and run any matching function
    (let ((modefunc (assq major-mode predictive-major-mode-alist)))
      (when modefunc
	(if (functionp (cdr modefunc))
	    (funcall (cdr modefunc))
	  (error "Wrong type argument: functionp, %s"
		 (prin1-to-string (cdr modefunc))))))
    
    ;; turn on which-dict mode
    (when predictive-which-dict (predictive-which-dict-mode t))
    
    ;; setup idle-timer to flush auto-learn and auto-add caches
    (setq predictive-flush-auto-learn-timer
	  (run-with-idle-timer predictive-flush-auto-learn-delay
			       t 'predictive-flush-auto-learn-caches))
    
    ;; initialise internal variables
    (predictive-reset-state)
    ;; run hook
    (run-hooks 'predictive-mode-hook))
)





;;; ================================================================
;;;       Public functions for completion in predictive mode

(defun turn-on-predictive-mode ()
  "Turn on predictive mode. Useful for adding to hooks."
  (unless predictive-mode
    (predictive-mode))
)



(defun predictive-complete-word-at-point ()
  "In predictive mode, complete the word at or next to the point.
How to select the resulting completions depends on the settings of variables
`predictive-dynamic-completion' and `predictive-offer-completions'."
  (interactive)
  
  ;; resolve any old completion that's hanging around
  (with-resolve-old-completion
   ;; re-complete the current completion if there is one
   (if predictive-typed-string
       (predictive-complete predictive-typed-string)
     
     ;; otherwise, check if there's a word at the point
     (if (null (thing-at-point 'word))
	 (when predictive-always-complete (predictive-complete ""))
       
       ;; move to the end of the word, unless already there
       (unless (and (char-after) (not (= ?w (char-syntax (char-after)))))
	 (forward-word 1))
       ;; set things up for completing the word
       (setq predictive-typed-string (thing-at-point 'word))
       (set-text-properties 0 (length predictive-typed-string) nil
			    predictive-typed-string)
       (move-overlay predictive-overlay (point) (point))
       (predictive-complete predictive-typed-string))
     ))
)




(defun predictive-self-insert ()
  "Execute a predictive mode function based on syntax.

Decide what predictive mode completion function to execute by looking up the
character's syntax in `predictive-syntax-alist'. The syntax-derived function
can be overridden for individual characters by
`predictive-override-syntax-alist'.

The default functions in `predictive-syntax-alist' all insert the last input
event, in addition to taking any completion related action \(hence the name,
`predictive-self-insert'\). Therefore, unless you know what you are doing, it
is best to only bind `predictive-self-insert' to printable characters."
  (interactive)
  
  ;; look up typed character in override-syntax-alist, and execute
  ;; associated function if it exists
  (let ((override (assq last-input-event predictive-override-syntax-alist)))
    (if override
	(if (functionp (cdr override))
	    (funcall (cdr override))
	  (error "Wrong type argument: functionp, %s"
		 (prin1-to-string (cdr override))))
      
      ;; if syntax isn't overridden, decide what to do based on typed
      ;; character's syntax
      (let ((syntax (assq (char-syntax last-input-event)
			  predictive-syntax-alist)))
	;; if syntax isn't explicitly defined, use default
	(unless syntax (setq syntax (assq t predictive-syntax-alist)))
	(if (functionp (cdr syntax))
	    (funcall (cdr syntax))
	  (error "Wrong type argument: functionp, %s"
		 (prin1-to-string (cdr syntax)))))))
)




(defun predictive-insert-and-complete ()
  "Insert the last input event and complete the resulting string.

The last input event should be a printable character. This function is
intended to be bound to printable characters in a keymap."
  (interactive)

  ;; resolve any old completion that's hanging around
  (with-resolve-old-completion
   ;; delete the current completion, if any
   (predictive-delete-completion)
   ;; insert the newly typed character and advance overlay
   (self-insert-command 1)
   (move-overlay predictive-overlay (point) (point))
   ;; add the newly typed character to the current string
   (setq predictive-typed-string
	 (concat predictive-typed-string (string last-input-event)))
   ;; try to complete the new string
   (predictive-complete predictive-typed-string))
)




(defun predictive-accept-and-newline (&optional n)
  "In predictive mode, insert a newline. Accepts the current completion if
there is one. If N is specified, insert that many newlines. Interactively, N
is the prefix argument."
  (interactive "p")

  (with-resolve-old-completion
   (predictive-accept)
   (newline)
   (when n (newline (1- n))))
)




(defun predictive-reject-and-newline (&optional n)
  "In predictive mode, insert a newline. Abandons the current completion if
there is one. If N is specified, insert that many newlines. Interactively, N
is the prefix argument."
  (interactive "p")
  
  (with-resolve-old-completion
   (predictive-abandon)
   (newline)
   (when n (newline (1- n))))
)




(defun predictive-backward-delete (&optional n)
  "In predictive mode, delete backwards N characters \(default 1\).

Remove N characters from the string currently being completed, and complete
the resulting shorter string. If N is larger than or equal to the length of
the string being completed, delete the entire string and abandon the
completion. If no completion is in progress, just delete the previous N
characters. Interactively, N is the prefix argument."
  (interactive "p")
  
  ;; resolve any old completion that's still hanging around
  (with-resolve-old-completion
   
   ;; if no completion is in progress, just delete the previous N chars
   (if (null predictive-typed-string)
       (backward-delete-char-untabify n)
     ;; if a completion is in progress...
     (let ((len (length predictive-typed-string))
	   (n (if (null n) 1 (abs n))))
       ;; if N is larger than the length of the typed string, delete the
       ;; typed string and abandon the completion
       (if (>= n len)
	   (progn
	     (predictive-delete-completion)
	     (backward-delete-char len)
	     (predictive-reset-state)
	   )
	 ;; otherwise, delete N characters from the typed string and
	 ;; re-complete the shorter string
	 (predictive-delete-completion)
	 (backward-delete-char n)
	 (setq predictive-typed-string
	       (substring predictive-typed-string 0 (- n)))
	 (move-overlay predictive-overlay (point) (point))
	 (predictive-complete predictive-typed-string)))
     ))
)




(defun predictive-backward-delete-and-complete (&optional n)
  "In predictive mode, delete backwards N characters \(default 1\),
and complete whatever string is then in front of the point, if
any. Interactively, N is the prefix argument."
  (interactive "p")
  
  ;; resolve any old completion that's hanging around
  (with-resolve-old-completion
   ;; delete the current completion, if any
   (predictive-delete-completion)
   
   (let ((len (length predictive-typed-string))
	 (n (if (null n) 1 (abs n))))
     (cond
      ;; if we're not deleting the entire existing typed string, delete n
      ;; characters and re-complete the shorter string
      ((< n len)
       (backward-delete-char n)
       (setq predictive-typed-string
	     (substring predictive-typed-string 0 (- n)))
       (move-overlay predictive-overlay (point) (point))
       (predictive-complete predictive-typed-string))
      
      ;; if we're deleting the entire existing typed string, but no more, just
      ;; delete it and reset the completion state
      ((= n len)
       (backward-delete-char n)
       (predictive-reset-state))
      
      ;; otherwise, delete n characters and if we've deleted "into" another
      ;; word, complete whatever string remains before the new point
      (t
       (backward-delete-char len)
       (backward-delete-char-untabify (- n len 1))
       ;; if we're deleting "into" a word, and having deleted there will still
       ;; be a string before the new point, then complete that string
       (if (and (char-before)
		(= (char-syntax (char-before)) ?w)
		(not (= (char-before) ?\t))
		(char-before (1- (point)))
		(= (char-syntax (char-before (1- (point)))) ?w))
	   (let (mark)
	     (predictive-reset-state)
	     (backward-delete-char-untabify 1)
	     (setq mark (point))
	     (backward-word 1)
	     (setq predictive-typed-string
		   (buffer-substring (point) mark))
	     (set-text-properties 0 (length predictive-typed-string) nil
				  predictive-typed-string)
	     (goto-char mark)
	     (predictive-complete predictive-typed-string))
	 
	 ;; if after deleting there won't be a string immediately before point,
	 ;; just delete and reset the completion state
	 (backward-delete-char-untabify 1)
	 (predictive-reset-state)))
      )))
)




(defun predictive-accept-and-insert ()
  "In predictive mode, accept current completion, and insert last input event,
which should be a single printable character. This function is intended to be
bound to printable characters in a keymap."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (with-resolve-old-completion
   ;; accept the completion
   (predictive-accept)
   ;; insert newly typed character
   (self-insert-command 1))
)




(defun predictive-accept ()
  "In predictive mode, accept the current completion."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (with-resolve-old-completion
   
   ;; if a completion is in progress...
   (when (overlay-start predictive-overlay)
     ;; set up variables storing current dictionary and completion
     (let ((dict (predictive-current-dict))
	   (word (concat predictive-typed-string
			 (when predictive-completion-num
			   (car (aref predictive-completions
				      predictive-completion-num))))))
       (when (dict-p dict) (setq dict (list dict)))
       (when (and predictive-ignore-initial-caps
		  (predictive-capitalized-p word))
	 (setq word (downcase word)))
       
       ;; if the completion was not in the dictionary, auto-add-to-dict is
       ;; enabled, and either add-to-dict-ask is disabled or user responded
       ;; "y" when asked, then add the new word to the appropriate
       ;; dictionary
       (if (null predictive-completion-num)
	   (when (and predictive-auto-add-to-dict
		      (or (not predictive-add-to-dict-ask)
			  (y-or-n-p
			   (format "Add word \"%s\" to dictionary? " word))
			  ))
	     (cond
	      
	      ;; if adding to the currently active dictionary, then do just
	      ;; that, adding to the first in the list if there are a list of
	      ;; dictionaries
	      ((eq predictive-auto-add-to-dict t)
	       (if predictive-use-auto-learn-cache
		   ;; if caching auto-learned words, do so
		   (puthash (cons word (car dict))
			    (1+ (gethash (cons word (car dict))
					 predictive-auto-add-cache 0))
			    predictive-auto-add-cache)
		 ;; otherwise, add it to the dictionary
		 (dict-insert (car dict) word)))
	      
	      ;; if adding to the buffer dictionary, add to the word list in
	      ;; the buffer, as well as to the buffer dictionary
	      ((eq predictive-auto-add-to-dict 'buffer)
	       (if predictive-use-auto-learn-cache
		   ;; if caching auto-added words, do so
		   (puthash (cons word 'buffer)
			    (1+ (gethash (cons word 'buffer)
					 predictive-auto-add-cache 0))
			    predictive-auto-add-cache)
		 ;; otherwise, add it to the dictionary
		 (predictive-add-to-buffer-dict word)))
	      
	      ;; anything else specifies an explicit dictionary to add to
	      (t
	       (setq dict (eval predictive-auto-add-to-dict))
	       ;; check `predictive-auto-add-to-dict' is a dictionary
	       (if (dict-p dict)
		 (if (not predictive-use-auto-learn-cache)
		     ;; if caching is off, add word to the dictionary
		     (dict-insert dict word)
		   ;; if caching is on, cache word
		   (puthash (cons word dict)
			    (1+ (gethash (cons word dict)
					 predictive-auto-add-cache 0))
			    predictive-auto-add-cache))
		 ;; display error message if not a dictionary
		 (beep)
		 (message "Word not added: `predictive-auto-add-to-dict'\
 does not evaluate to a dictionary")))
	      ))

	 
	 ;; if the completion was in the dictionary and auto-learn is set...
	 (when predictive-auto-learn
	   ;; if caching auto-learned words, do so
	   (if predictive-use-auto-learn-cache
	       (puthash (cons word dict)
			(1+ (gethash (cons word dict)
				     predictive-auto-learn-cache 0))
			predictive-auto-learn-cache)
	   ;; if not caching, search for the first dictionary containing the
	   ;; completion and increment its weight
	   (catch 'learned
	     (dotimes (i (length dict))
	       (when (dict-member-p (nth i dict) word)
		 (dict-insert (nth i dict) word)
		 (throw 'learned t))))))
	 ))
     
     
     ;; move point to the end of the completion
     (goto-char (overlay-end predictive-overlay)))
   ;; reset completion state
   (predictive-reset-state))
)




(defun predictive-abandon-and-insert ()
  "In predictive mode, abandon current completion, and insert last input
event, which should be a single printable character. This function is intended
to be bound to printable characters in a keymap."
  (interactive)
  
  ;; abandon the current completion
  (predictive-abandon)
  ;; insert the newly typed character
  (self-insert-command 1)
)




(defun predictive-abandon ()
  "In predictive mode, abandon the current completion."
  (interactive)
  
  ;; resolve any old completion that's hanging around
  (predictive-resolve-old-completion)
  ;; delete the current completion, if any
  (predictive-delete-completion)
  ;; reset completion state
  (predictive-reset-state)
)




(defun predictive-cycle (&optional n)
  "In predictive mode, cycle through available completions.

Optional argument N specifies the number of completions to cycle forwards
\(backwards if negative\). Default is 1. Interactively, N is the prefix
argument."
  (interactive "P")
  (when (null n) (setq n 1))
  
  ;; resolve any old completion that's still hanging around
  (with-resolve-old-completion
   
   ;; don't do anything if no completion is in progress
   (when predictive-typed-string
     
     ;; if no completions were found for the string currently being completed,
     ;; display a message to that effect
     (if (null predictive-completions)
	 (message "No completions available")
     
       ;; delete current completion
       (predictive-delete-completion)
       ;; insert the next completion
       (goto-char (overlay-start predictive-overlay))
       (setq predictive-completion-num
	     (if predictive-completion-num
		 (mod (+ predictive-completion-num n)
		      (length predictive-completions))
	       0))
       (let ((completion
	      (car (aref predictive-completions predictive-completion-num))))
	 (goto-char (overlay-start predictive-overlay))
	 (insert completion)
	 ;; extend the overlay over the new completion
	 (move-overlay predictive-overlay (overlay-start predictive-overlay)
		 (+ (overlay-start predictive-overlay) (length completion)))
	 ;; return point to end of typed string
	 (goto-char (overlay-start predictive-overlay)))
       
       ;; if offer completions in on, display a list of possible completions
       ;; in the echo area
       (when predictive-offer-completions
	 (let ((msg ""))
	   (dotimes (i (min (length predictive-offer-completions-keylist)
			    (length predictive-completions)))
	     (setq msg (concat msg
			  (format "(%s) %s  "
				  (nth i predictive-offer-completions-keylist)
				  (concat
				   predictive-typed-string
				   (car (aref predictive-completions i)))))))
	   (message msg)))
       (setq predictive-completions-on-offer t)
       )))
)




(defun predictive-select-completion ()
  "In predictive mode, select a completion to insert from the list
of possible completions. Intended to be bound in
`predictive-offer-completions-map' keymap, for use when
`predictive-offer-completions' is enabled."
  (interactive)
  
  ;; look up typed character to find associated completion number
  (let ((n (position (string last-input-event)
		     predictive-offer-completions-keylist :test 'equal)))
    
    (cond
     ;; if character was not found, key bindings are messed up
     ((null n)
      (setq predictive-completions-on-offer nil)
      (error "Key bindings are inconsistent with\
 `predictive-offer-completions-keylist'"))
     
     ;; if point has moved outside current completion, resolve the old
     ;; completion and unread the number so it can be re-read as a normal
     ;; charater
     ((predictive-resolve-old-completion)
      (setq unread-command-events
	    (append (list last-input-event) unread-command-events)))
     
     ;; if no completions are available, display a message to that effect
     ((null predictive-completions)
      (beep)
      (message "No completions available")
      (setq predictive-completions-on-offer nil))
     
     ;; if trying to insert completion number that is larger than number of
     ;; available completions, display a message to that effect, but still
     ;; offer the completions for selection
     ((>= n (length predictive-completions))
      (beep)
      (message "Only %d completions available, so can't select completion %d"
	       (length predictive-completions) n)
      (let ((msg ""))
	(dotimes (i (min (length predictive-offer-completions-keylist)
			 (length predictive-completions)))
	  (setq msg (concat msg
		      (format "(%s) %s  "
			      (nth i predictive-offer-completions-keylist)
			      (concat predictive-typed-string
				      (car (aref predictive-completions i)))
			      ))))
	(sit-for 2)
	(message msg)))
     
     
     ;; if we're actually going to do something...
     (t
      ;; delete any existing completion
      (predictive-delete-completion)
      ;;  insert the selected completion, and set various variables so
      ;;  that the subsequent functions behave properly
      (let ((completion (car (aref predictive-completions n))))
	(insert completion)
	(move-overlay predictive-overlay
		      (- (point) (length completion)) (point))
	(setq predictive-completion-num n))
      ;; if accept-on-select is t, accept the current completion,
      ;; otherwise scoot-ahead
      (if predictive-accept-on-select (predictive-accept)
	(predictive-scoot-ahead))))
    )
)




(defun predictive-scoot-ahead ()
  "In predictive mode, insert the current completion,
and look for completions of the resulting string."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (predictive-resolve-old-completion)
  
  ;; don't do anything unless a completion has been inserted
  (when predictive-completion-num
    ;; move point and overlay to end of completion
    (goto-char (overlay-end predictive-overlay))
    (move-overlay predictive-overlay (overlay-end predictive-overlay)
		  (overlay-end predictive-overlay))
    ;; completion becomes part of typed string
    (setq predictive-typed-string
       (concat predictive-typed-string
	   (car (aref predictive-completions predictive-completion-num))))
    ;; re-complete on new string
    (predictive-complete predictive-typed-string))
)




(defun predictive-scoot-or-accept ()
  "In predictive mode, insert the current completion
if point is not already at the end of the current completion, and look for
completions of the resulting string. If point is already at the end, accept
the completion."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (predictive-resolve-old-completion)
  
  ;; accept current completion if point is already at the end of it, otherwise
  ;; scoot ahead
  (if (= (point) (overlay-end predictive-overlay))
      (predictive-accept)
    (predictive-scoot-ahead))
)




(defun predictive-scoot-or-accept-and-insert ()
  "In predictive mode, insert the current completion
if point is not already at the end of the current completion, and look for
completions of the resulting string. If point is already at the end, accept
the completion and insert the last input event, which should be a printable
character. Intended to be bound to printable characters in a keymap."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (predictive-resolve-old-completion)
  
  ;; accept current completion if point is already at the end of it, otherwise
  ;; scoot ahead
  (if (= (point) (overlay-end predictive-overlay))
      (predictive-accept-and-insert)
    (predictive-scoot-ahead))
)



(defun predictive-scoot-or-cycle ()
  "In predictive mode, insert the current completion
if point is not at the end of the current completion, and look for completions
of the resulting string. If point is already at the end, cycle the current
completion."
  (interactive)
  
  ;; resolve any old completion that's still hanging around
  (predictive-resolve-old-completion)
  
  ;; cycle current completion if point is already at the end of it, otherwise
  ;; scoot ahead
  (if (= (point) (overlay-end predictive-overlay))
      (predictive-cycle)
    (predictive-scoot-ahead))
)



(defun predictive-jump-to-current-completion ()
  "In predictive mode, jump to the current completion."
  (interactive)
  (if predictive-typed-string
      (goto-char (overlay-start predictive-overlay))
    (message "No completion in progress"))
)



(defun predictive-jump-to-old-completion ()
  "In predictive mode, jump to the last completion
that was automatically accepted or abandoned because the had point moved."
  (interactive)
  (if predictive-old-completion-marker
      (goto-char predictive-old-completion-marker)
    (message "No old completion to jump to"))
)






;;; ================================================================
;;;           Internal functions to do with completion


(defun predictive-complete (string)
  ;; Main completion function.
  ;; Try to complete string STRING, which should probably be before the
  ;; point. If any completions are found, the subsequent behaviour depends on
  ;; the settings of the variables `predictive-dynamic-completion' and
  ;; `predictive-offer-completions'.
  
  ;; don't try to complete nothing! (Note: an empty string is something)
  (when string
    
    ;; resolve any old completion that's still hanging around
    (with-resolve-old-completion
     ;; delete the current completion, if any
     (predictive-delete-completion)
     
     (let ((str string))
       ;; if `predictive-ignore-initial-caps' is enabled and first character of
       ;; string is capitalized, also search for completions for uncapitalized
       ;; version
       (when (and predictive-ignore-initial-caps
		  (predictive-capitalized-p str))
	 (setq str (list str (downcase str))))
       
       ;; try to complete string, and store the ordered vector of completions
       ;; in the `predictive-completions' variable
       (if (null
	    (setq predictive-completions
		  (dict-complete-ordered (predictive-current-dict) str
					 predictive-max-completions)))
	   
	   ;; if no completion was found, store that fact and don't offer any
	   (progn
	     (setq predictive-completion-num nil)
	     (setq predictive-completions-on-offer nil))
	 
	 
	 ;; if completions were found, move the overlay to point
	 (move-overlay predictive-overlay (point) (point))
	 ;; if dynamic completion is on, insert the best completion
	 (when predictive-dynamic-completion
	   (let ((completion (car (aref predictive-completions 0))))
	     (insert completion)
	     (setq predictive-completion-num 0)
	     ;; extend overlay over the new completion
	     (move-overlay predictive-overlay
			   (overlay-start predictive-overlay)
			   (+ (overlay-start predictive-overlay)
			      (length completion)))
	     ;; return point to end of typed string
	     (goto-char (overlay-start predictive-overlay))))
	 
	 ;; if offer completions in on, display a list of possible completions
	 ;; in the echo area
	 (when predictive-offer-completions
	   (let ((msg ""))
	     (dotimes (i (min (length predictive-offer-completions-keylist)
			      (length predictive-completions)))
	       (setq msg (concat msg
			    (format "(%s) %s  "
			       (nth i predictive-offer-completions-keylist)
			       (concat predictive-typed-string
				       (car (aref predictive-completions i)))
			       ))))
	     (message msg)))
	 (setq predictive-completions-on-offer t))
       )))
)




(defmacro predictive-capitalized-p (string)
  ;; Return t if string is capitalized (only first letter upper case), nil
  ;; otherwise.
  `(and (> (length ,string) 0)
	(= (aref ,string 0) (upcase (aref ,string 0)))
	(not (= (aref ,string 0) (downcase (aref ,string 0))))
	(or (= 1 (length ,string))
	    (string= (substring ,string 1) (downcase (substring ,string 1)))))
)




(defun predictive-resolve-old-completion ()
  ;; Resolve the old completion, if any. If there is no current completion, or
  ;; if the point is still within the completion (completion is still
  ;; \"current\"), do nothing and return nil. If the point has moved
  ;; elsewhere, accept or abandon any completion according to the setting of
  ;; `predictive-accept-if-point-moved', reset the completion state, and
  ;; return non-nil"
  
  ;; don't do anything and return nil if there is no completion in progress or
  ;; point is still within the current completion
  (if (not (and (overlay-start predictive-overlay)
		(or (< (point) (overlay-start predictive-overlay))
		    (> (point) (overlay-end predictive-overlay)))))
      nil   ; return nil to indicate there was no old completion to resolve
    
      
    ;; if point has moved elsewhere and `predictive-accept-if-point-moved' is
    ;; set, accept the old completion and reset the completion state
    (if predictive-accept-if-point-moved
	(progn
	  (setq predictive-old-completion-marker
		(copy-marker (overlay-start predictive-overlay)))
	  (predictive-reset-state)
	  (message (substitute-command-keys
		    "Previous completion accepted because point has\
 moved. Use \"\\[predictive-jump-to-old-completion]\" to return to it.")))
      
      ;; if point has moved elsewhere, a completion was inserted, and
      ;; `predictive-accept-if-point-moved' is NOT set, abandon the old
      ;; completion and reset the completion state
      (setq predictive-old-completion-marker
	    (copy-marker (overlay-start predictive-overlay)))
      (predictive-delete-completion)
      (predictive-reset-state)
      (message (substitute-command-keys
		"Previous completion abandoned because point has moved. Use\
 \"\\[predictive-jump-to-old-completion]\" to return to it.")))
    
    t)  ; return t to indicate an old completion was resolved
)




(defmacro with-resolve-old-completion (&rest body)
  ;; Use this instead of the `predictive-resolve-old-completion' function if
  ;; the calling function is going to display any messages.
  
  ;; if this is the outter invocation of this macro, resolve any old
  ;; completion that's hanging around and temporarily display the appropriate
  ;; message after the calling function has done its stuff
  `(let (msg)
     (unless (boundp 'inside-with-resolve-old-completion)
       (setq msg (predictive-resolve-old-completion)))
     
     (let (inside-with-resolve-old-completion) ,@body)
     
     (when (and msg (not (boundp 'inside-with-resolve-old-completion)))
       (setq msg (current-message))
       ;; (sit-for 1)
       (if predictive-accept-if-point-moved
	   (message (substitute-command-keys
		     "Previous completion accepted because point has\
 moved. Use \"\\[predictive-jump-to-old-completion]\" to return to it."))
	 (message (substitute-command-keys
		   "Previous completion abandoned because point has\
 moved. Use \"\\[predictive-jump-to-old-completion]\" to return to it.")))
       (sit-for 2)
       (message msg)))
)




(defmacro predictive-delete-completion ()
  ;; Delete any completion inserted by dynamic completion mode.
  '(when predictive-completion-num
     (delete-region (overlay-start predictive-overlay)
		    (overlay-end predictive-overlay))
     (move-overlay predictive-overlay (overlay-start predictive-overlay)
		   (overlay-start predictive-overlay)))
)



(defun predictive-reset-state ()
  ;; Reset variables to their initial state, ready for a new completion process
  (setq predictive-typed-string nil)
  (setq predictive-completion-num nil)
  (setq predictive-completions nil)
  (setq predictive-completions-on-offer nil)
  (delete-overlay predictive-overlay)
  (when predictive-always-complete (predictive-complete ""))
)



(defun predictive-flush-auto-learn-caches ()
  ;; Flush all entries in the auto-learn and auto-add hash tables, adding them
  ;; to the appropriate dictionary

  (when predictive-mode
    ;; map over all words in auto-learn cache
    (maphash
     (lambda (key weight)
       (let ((word (car key))
	     (dict (cdr key)))
	 ;; add word to whichever dictionary it is found in
	 (when (dict-p dict) (setq dict (list dict)))
	 (catch 'learned
	   (dotimes (i (length dict))
	     (when (dict-member-p (nth i dict) word)
	       (dict-insert (nth i dict) word weight)
	       (throw 'learned t))))))
     predictive-auto-learn-cache)
    (clrhash predictive-auto-learn-cache)
    
    ;; map over all words in auto-add cache
    (maphash
     (lambda (key weight)
       (let ((word (car key))
	     (dict (cdr key)))
	 ;; if word should be added to buffer dictionary, do so
	 (if (eq dict 'buffer)
	     (predictive-add-to-buffer-dict word weight)
	   ;; otherwise add it to whichever dictionary is stored in the cache
	   (dict-insert dict word weight))))
     predictive-auto-add-cache)
    (clrhash predictive-auto-add-cache))
)





;;; ================================================================
;;;       Public functions for predictive mode dictionaries

(defun predictive-load-dict (dict)
  "Load the dictionary DICTNAME and associate it with the current buffer.

DICT must be the name of a dictionary to be found somewhere in the load
path. Interactively, it is read from the mini-buffer."
  (interactive "sDictionary to load: \n")
  (when (stringp dict) (setq dict (intern dict)))
  
  (if (require dict nil t)
      (setq predictive-used-dict-list
	    (cons (eval dict) predictive-used-dict-list))
    (error "Dictionary %s not found" (prin1-to-string dict)))
)




(defun predictive-add-to-dict (dict word &optional weight)
  "Insert WORD into predictive mode dictionary DICT.

Optional argument WEIGHT sets the weight. If the word is not in the
dictionary, it will be added to the dictionary with initial weight WEIGHT \(or
0 if none is supplied\). If the word is already in the dictionary, its weight
will be incremented by WEIGHT \(or by 1 if WEIGHT is not supplied).

Interactively, WORD and DICT are read from the minibuffer, and WEIGHT is
specified by the prefix argument."
  (interactive "SDictionary to add to: \nsWord to add \(can include\
 spaces and other punctuation characters\): \nP")
  
  (dict-insert dict word weight)
)




(defun predictive-create-dict (dict &optional file populate autosave speed)
  "Create a new predictive mode dictionary called DICT.

If POPULATE is not specified, create an empty dictionary. If POPULATE is
specified, populate the dictionary from that file \(see
`dict-populate-from-file').

The optional argument FILE specifies a file to associate with the
dictionary. The dictionary will be saved to this file by default \(similar to
the way a file is associated with a buffer).

If the optional argument AUTOSAVE is t, the dictionary will automatically be
saved when it is unloaded. If nil, all unsaved changes are lost when it is
unloaded. Defaults to `predictive-dict-autosave'.

The optional argument SPEED sets the desired speed with which string should be
completed using the dictionary, in seconds. It defaults to
`predictive-completion-speed'.

Interactively, DICT and FILE are read from the minibuffer. SPEED and AUTOSAVE
use the defaults provided by `predictive-completion-speed' and
`predictive-dict-autosave' respectively."
  
  (interactive "SDictionary name: \nFAssociated filename \(optional): \n\
fFile to populate from \(leave blank to create an empty dictionary\): ")
  (unless (or (null populate) (file-regular-p populate))
    (setq populate nil))
  
  (let ((complete-speed (if speed speed predictive-completion-speed))
	(autosave (if autosave autosave predictive-dict-autosave))
	;; the insertion function inserts a weight if none already exists,
	;; otherwise it adds the new weight to the existing one, or if supplied
	;; weight is nil, incremenets existing weight
	(insfun '(lambda (weight data)
		   (cond ((not (or weight data)) 0)
			 ((null weight) (1+ data))
			 ((null data) weight)
			 (t (+ weight data)))))
	;; the rank function compares by weight (larger is "better"), failing
	;; that by string length (smaller is "better"), and failing that it
	;; compares the strings alphabetically
	(rankfun '(lambda (a b)
		    (if (= (cdr a) (cdr b))
			(if (= (length (car a)) (length (car b)))
			    (string< (car a) (car b))
			  (< (length (car a)) (length (car b))))
		      (> (cdr a) (cdr b))))))
    
    ;; create the new dictionary
    (set dict (dict-create dict file autosave
			   nil nil complete-speed nil insfun rankfun))
    ;; populate it
    (if (null populate)
	(message "Created dictionary %s" dict)
      (dict-populate-from-file (eval dict) populate)
      (message "Created dictionary %s and populated it from file %s"
	       dict populate)))
)




(defun predictive-add-to-buffer-dict (string &optional weight)
  "Add STRING to the predictive mode buffer-local dictionary,
and to the word list at the end of the current buffer. Interactively, STRING
is read from the mini-buffer and weight is specified by the prefix argument."
  (interactive "sString to add: \nP")
  
  ;; add string and weight to dictionary
  (dict-insert predictive-buffer-dict string weight)
  
  (save-excursion
    ;; look for comment marking location of buffer word list, and create it if
    ;; none already exists
    (goto-char (point-min))
    (if (re-search-forward "predictive-mode local words:" nil t)
	(forward-line 1)
      (goto-char (point-max))
      (insert "\n\n" comment-start " predictive-mode local words:\n"))
    
    ;; find correct position to insert string in buffer word list
    (let (entry (w (if (numberp weight) weight 0)))
      (while (and (setq entry (dict-read-line))
		  (string< (car entry) string))
	(forward-line 1))
      ;; if string is already in word list, increment its weight
      (when (string= (car entry) string)
	(if weight (setq w (+ weight (cdr entry))) (setq w (1+ (cdr entry))))
	(kill-line 1))
      (insert comment-start " \"" string "\" ")
      (insert (number-to-string w))
      (insert "\n")))
)




(defun predictive-learn-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the buffer. DICT can either be a dictionary, or
a list of dictionaries. If DICT is not supplied, it defaults to all
dictionaries used by BUFFER. However, DICT must be supplied if ALL is
specified (see below).

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be sprecified.

Interactively, BUFFER and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument.

See also `predictive-fast-learn-from-buffer'."

  (interactive "bBuffer to learn from: \nsDictionary to update (defaults to\
 all in use): \nP")
  
  (let ((i 0) (d 0) numdicts dictname restore-mode regexp currdict)
    (save-excursion
      
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode t)))
      
      ;; default to all dictionaries used by the buffer
      (if (or (null dict) (string= dict ""))
	  (if all
	      (error "Argument ALL supplied but no dictionary specified")
	    (setq dict predictive-used-dict-list))
	(unless (setq dict (intern-soft dict))
	  (error "Dictionary %s could not be found" dict)))
      (when (dict-p dict) (setq dict (list dict)))
      (setq numdicts (length dict))
      
      ;; map over all dictionaries in dictionary list
      (mapc
       (lambda (dic)
	 ;;initialise counters etc.for messages
	 (setq dictname (dic-name dic))
	 (setq d (1+ d))
	 (setq i 0)
	 (message "Learning words for dictionary %s\
 (dict %d of %d, word 1)..." dictname d numdicts) 
	 
	 ;; map over all words in dictionary
	 (dict-map
	  (lambda (word weight)   ; (value passed to weight is ignored)
	    ;; construct regexp for word
	    (setq regexp (regexp-quote word))
	    (when (= ?w (char-syntax (aref word 0)))
	      (setq regexp (concat "\\b" regexp)))
	    (when (= ?w (char-syntax (aref word (1- (length word)))))
	      (setq regexp (concat regexp "\\b")))
	    ;; count occurences of current word
	    (setq weight 0)
	    (goto-char (point-min))
	    (while (re-search-forward regexp nil t)
	      (if all
		  (setq weight (1+ weight))
		;; if ALL is nil, only count occurence if the active
		;; dictionary at that location matches the dictionary we're
		;; working on
		(setq currdict (predictive-current-dict))
		(when (or (and (listp currdict) (memq dic currdict))
			  (eq dic currdict))
		  (setq weight (1+ weight)))))
	    ;; increment word's weight
	    (dict-insert dic word weight)
	    (when (= 0 (mod (setq i (1+ i)) 10))
	      (message "Learning words for dictionary %s\
 (dict %d of %d, word %d)..." dictname d numdicts i)))
	  dic)   ; map over all words in dic
	 
	 (message "Learning words for dictionary %s\
 (dict %d of %d)...done" dictname d numdicts))
       dict)     ; map over dictionaries in dict list
      
      ;; restore predictive-mode state
      (unless (or all restore-mode) (predictive-mode nil))
    ))
)




(defun predictive-learn-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the file. DICT can either be a dictionary, or a
list of dictionaries. If DICT is not supplied, it defaults to all dictionaries
used by FILE. However, DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be specified.

Interactively, FILE and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument."

  (interactive "fFile to learn from: \nsDictionary to update (required if
 prefix argument supplied): \nP")
  
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-learn-from-buffer buff dict all)
      (unless visiting (kill-buffer buff))))
)




(defun predictive-fast-learn-from-buffer (&optional buffer dict all)
  "Learn word weights from BUFFER (defaults to the current buffer).

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the buffer. DICT can either be a dictionary, or
a list of dictionaries. If DICT is not supplied, it defaults to all
dictionaries used by BUFFER. However, DICT must be supplied if ALL is
specified, see below.

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be sprecified.

Interactively, BUFFER and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-buffer' for large
dictionaries, but will miss any words not consisting entirely of word- or
symbol-constituent characters according to the buffer's syntax table."
  
  (interactive "bBuffer to learn from: \nsDictionary to update (defaults to\
 all in use): \nP")
  
  (let (restore-mode currdict word percent)
    (save-excursion
      ;; switch on predictive mode in the buffer if necessary
      (when buffer (set-buffer buffer))
      (unless all
	(if predictive-mode (setq restore-mode t) (predictive-mode t)))
      
      ;; default to all dictionaries used by buffer
      (if (or (string= dict "") (null dict))
	  (if all
	      (error "Argument ALL supplied but no dictionary specified")
	    (setq dict nil))
	(unless (setq dict (intern-soft dict))
	  (error "Dictionary %s could not be found" dict)))
      
      
      ;; step through each word in buffer...
      (goto-char (point-min))
      (setq percent 0)
      (message "Learning words (0%%)...")
      (while (re-search-forward "\\b\\(\\sw\\|\\s_\\)+\\b" nil t)
	(setq word (match-string 0))
	(when (and predictive-ignore-initial-caps
		   (predictive-capitalized-p word))
	  (setq word (downcase word)))
	(cond
	 ;; if ALL was specified, learn current word
	 (all
	  (when (dict-member-p dict (match-string 0))
	    (dict-insert dict (match-string 0))))
	 ;; if ALL was not specified and a dictionary has been specified, only
	 ;; increment the current word's weight if dictionary is active there
	 (dict
	  (setq currdict (predictive-current-dict))
	  (when (and (or (and (listp currdict) (memq dict currdict))
			 (eq dict currdict))
		     (dict-member-p dict word))
	    (dict-insert dict word)))
	 ;; if ALL is not specified and no dictionary was specified, increment
	 ;; its weight in first dictionary active there that contains the word
	 (t
	  (setq currdict (predictive-current-dict))
	  (when (dict-p currdict) (setq currdict (list currdict)))
	  (catch 'learned
	    (dotimes (i (length currdict))
	      (when (dict-member-p (nth i currdict) word)
		(dict-insert (nth i currdict) word)
		(throw 'learned t))))))
	
	(when (/= (/ (* (point) 100) (point-max)) percent)
	  (setq percent (/ (* (point) 100) (point-max)))
	  (message "Learning words (%d%%)..." percent))
      )  ; end while loop
      
      (unless (or all restore-mode) (predictive-mode nil))
      (message "Learning words...done")))
)



(defun predictive-fast-learn-from-file (file &optional dict all)
  "Learn word weights from FILE.

The word weight of each word in dictionary DICT is incremented by the number
of occurences of that word in the file. DICT can either be a dictionary, or a
list of dictionaries. If DICT is not supplied, it defaults to all dictionaries
used by FILE. However, DICT must be supplied if ALL is specified, see below.

By default, only occurences of a word that occur in a region where the
dictionary is active are taken into account. If optional argument ALL is
non-nil, all occurences are taken into account. In this case, a dictionary
must be specified.

Interactively, FILE and DICT are read from the mini-buffer, and ALL is
specified by the presence of a prefix argument.

This function is faster then `predictive-learn-from-file' for large
dictionaries, but will miss any words not consisting entirely of word- or
symbol-constituent characters."

  (interactive "fFile to learn from: \nsDictionary to update (required if
 prefix argument supplied): \nP")
  
  (save-excursion
    ;; open file in a buffer
    (let (visiting buff)
      (if (setq buff (get-file-buffer file))
	  (setq visiting t)
	(find-file file))
      ;; learn from the buffer
      (predictive-fast-learn-from-buffer buff dict all)
      (unless visiting (kill-buffer buff))))
)





;;; ===================================================================
;;;    Internal functions and variables to do with predictive mode
;;;    dictionaries


(defun predictive-current-dict (&optional point)
  ;; Return the currently active dictionary at POINT (or the point, if nil).
  (when (null point) (setq point (point)))
  (let (overlay-list overlay p p1)

    ;; if using switch-dictionary auto-overlays, find highest priority overlay
    (setq overlay-list
	  (auto-overlays-at-point point
				  '((identity dict) (identity auto-overlay))))
    (setq overlay (pop overlay-list))
    (dolist (o overlay-list)
      (setq p (overlay-get overlay 'priority))
      (setq p1 (overlay-get o 'priority))
      (when (or (and (null p) p1)
		(and p p1 (> p1 p))
		(and (equal p1 p)
		     (> (overlay-start o) (overlay-start overlay))))
	(setq overlay o)))
    
    ;; return the active dictionary
    (let ((dict (when overlay (overlay-get overlay 'dict))))
      ;; if there's a switch-dictionary overlay, and it doesn't set the
      ;; dictionary to 'main, return the dictionary it sets
      (if (and dict (not (eq dict 'predictive-main-dict)))
	  (cond
	   ((eq dict t))
	   ((symbolp dict) (eval dict))
	   ((dict-p dict) dict))
	;; otherwise, return the main and buffer dictionaries
	(if (listp predictive-main-dict)
	    (append (mapcar 'eval predictive-main-dict)
		    predictive-buffer-dict ())
	  (list (eval predictive-main-dict) predictive-buffer-dict))))
    )
)




(defun predictive-create-buffer-dict ()
  ;; Create the buffer-local predictive mode dictionary, and fill it with
  ;; words from the word list at the end of the current buffer (if it exists).
  
  ;; The insertion function inserts a weight if none already exists, otherwise
  ;; it adds the new weight to the existing one, or if supplied weight is nil,
  ;; incremenets existing weight
  (let ((insfun '(lambda (weight data)
		   (cond ((not (or weight data)) 0)
			 ((null weight) (1+ data))
			 ((null data) weight)
			 (t (+ weight data)))))
	;; the rank function compares by weight (larger is "better"), failing
	;; that by string length (smaller is "better"), and failing that it
	;; compares the strings alphabetically
	(rankfun '(lambda (a b)
		    (if (= (cdr a) (cdr b))
			(if (= (length (car a)) (length (car b)))
			    (string< (car a) (car b))
			  (< (length (car a)) (length (car b))))
		      (> (cdr a) (cdr b))))))
    
    ;; initialise the buffer dictionary
    (setq predictive-buffer-dict
	  (dict-create '*buffer* "" nil
		       nil nil predictive-completion-speed
		       nil insfun rankfun))
  )
  
  ;; look for buffer-local word list in current buffer
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "predictive-mode local words:" nil t)
      (forward-line)
      (let (entry)
	(while (setq entry (dict-read-line))
	  (dict-insert predictive-buffer-dict (car entry) (cdr entry))
	  (forward-line)))))
)




;;; ==================================================================
;;;       Functions and variables to do with which-dict mode

;; Stores current dictionary name for display in mode line
(defvar predictive-which-dict-name nil)
(make-variable-buffer-local 'predictive-which-dict-name)


;; Store buffer and point for which last dictionary name update was performed
(defvar predictive-which-dict-last-update nil)


;; Stores idle-timer that updates the current dictionary name
(defvar predictive-which-dict-timer nil)
(make-variable-buffer-local 'predictive-which-dict-timer)



(define-minor-mode predictive-which-dict-mode
    "Toggle predictive mode's which dictionary mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

Note that simply setting the minor-mode variable
`predictive-which-dict-mode' is not sufficient to enable
predictive mode."

    ;; initial value, mode-line indicator, and keymap
    nil nil nil

    ;; if which-dict mode has been turned on, setup the timer to update the
    ;; mode-line indicator
    (if predictive-which-dict-mode
	(setq predictive-which-dict-timer
	      (run-with-idle-timer 0.1 t 'predictive-update-which-dict))
      
      ;; if which-dict mode has been turned off, cancel the timer and reset
      ;; variables
      (when predictive-which-dict-timer
	(cancel-timer predictive-which-dict-timer)
	(setq predictive-which-dict-timer nil)
	(setq predictive-which-dict-last-update nil)
	(setq predictive-which-dict-name nil)))
)

      


(defun predictive-update-which-dict ()
  ;; Updates the `predictive-which-dict-name' variable used in the mode
  ;; line. Runs automatically from an idle timer setup by the minor mode
  ;; function.

  ;; only run if predictive mode is enabled and point has moved since last run
  (unless (or (null predictive-which-dict-mode)
	      (and (eq (current-buffer)
		       (car predictive-which-dict-last-update))
		   (eq (point) (cdr predictive-which-dict-last-update))))
    
    ;; store buffer and point at which update is being performed
    (setq predictive-which-dict-last-update (cons (current-buffer) (point)))
    
    (let ((dict (predictive-current-dict)) name str)
      ;; get current dictionary name(s)
      (if (dict-p dict) (setq name (dict-name dict))
	(setq name (mapconcat
		    (lambda (d) (if (stringp (dict-name d)) (dict-name d)
				  (symbol-name (dict-name d))))
		    dict ",")))
      
      ;; filter string to remove "-dict-" and "-predictive-"
      (while (string-match "-*dict-*\\|-*predictive-*" name)
	(setq name (replace-match "" nil nil name)))
      
      ;; if dictionary name has changed, update the mode line
      (unless (string= name predictive-which-dict-name)
	(setq predictive-which-dict-name name)
	(force-mode-line-update))))
)




;;; ================================================================
;;;              Setup function for normal English text

(defun predictive-setup-english ()
  "Sets up predictive mode for typing normal english text."
  (interactive)
  
  ;; use english dictionary
  (predictive-load-dict 'dict-english)
  (set (make-local-variable 'predictive-main-dict) 'dict-english)
  
  ;; set the syntax-derived funtions
  (set (make-local-variable 'predictive-syntax-alist) (list 
    ;; word constituents add to current completion
    (cons ?w 'predictive-insert-and-complete)
    ;; whitespace and punctuation chars accept current completion
    (cons ?  'predictive-accept-and-insert)
    (cons ?. 'predictive-accept-and-insert)
    ;; anything else abandons the current completion
    (cons t  'predictive-abandon-and-insert)))
  
  ;; - and & are symbols in text mode but we override them to make them
  ;; behave like word constituents
  (set (make-local-variable 'predictive-override-syntax-alist) (list
    (cons ?- 'predictive-insert-and-complete)
    (cons ?& 'predictive-insert-and-complete)))
)



;; (defun predictive-setup-english-alt ()
;;   "Sets up predictive mode for typing normal english text."
;;   (interactive)
  
;;   ;; use english dictionary
;;   (predictive-load-dict 'dict-english)
;;   (set (make-local-variable 'predictive-main-dict) 'dict-english)
  
;;   ;; set the syntax-derived funtions
;;   (set (make-local-variable 'predictive-syntax-alist) (list 
;;     ;; word constituents add to current completion
;;     (cons ?w 'predictive-insert-and-complete)
;;     ;; whitespace and punctuation chars abandon current completion
;;     (cons ?  'predictive-abandon-and-insert)
;;     (cons ?. 'predictive-abandon-and-insert)
;;     ;; anything else abandons the current completion
;;     (cons t  'predictive-abandon-and-insert)))
  
;;   ;; - and & are symbols in text mode but we override them to make them
;;   ;; behave like word constituents
;;   (set (make-local-variable 'predictive-override-syntax-alist) (list
;;     (cons ?- 'predictive-insert-and-complete)
;;     (cons ?& 'predictive-insert-and-complete)))
;; )
  




;;; ================================================================
;;;                     Initialise variables etc.

;; Set the default dictionary if it hasn't already been set (most likely in an
;; init file or setup function)
(unless predictive-main-dict
  (predictive-load-dict 'dict-english)
  (setq predictive-main-dict 'dict-english)
)



;; Set the 'predictive-completing-map' keymap (used during a completion
;; process), unless it's already been set (most likely in an init file or
;; setup function)
(unless predictive-completing-map
  (let ((map (make-sparse-keymap)))
    
    ;; <tab> scoots ahead
    (define-key map "\t" 'predictive-scoot-or-cycle)
    
    ;; M-<tab> cycles
    (define-key map [?\M-\t] 'predictive-cycle)
    
    ;; M-<shift>-<tab> cycles backwards
    (define-key map '[(meta shift iso-lefttab)] (lambda () "Cycle backwards
through completions." (interactive) (predictive-cycle -1)))
    
    ;; M-<space> abandons
    (define-key map "\M- " 'predictive-abandon)
    
;;     ;; M-<space> accepts
;;     (define-key map "\M- " 
;;       (lambda () "Accept the current completion and insert a space."
;; 	(interactive)
;; 	(with-resolve-old-completion (predictive-accept) (insert " "))))
    
    ;; RET accepts the current completion and inserts a newline
    (define-key map "\r" 'predictive-accept-and-newline)
    
;;     ;; RET abandons the current completion and inserts a newline
;;     (define-key map "\r"
;;       (lambda () "Accept the current completion and insert a newline."
;; 	(interactive)
;; 	(with-resolve-old-completion (predictive-abandon) (newline))))
    
    (setq predictive-completing-map map))
)



;; Set the list of characters used to select completions when offer-completios
;; is on, if it hasn't been set already (most likely in an init file)
(unless predictive-offer-completions-keylist
  (setq predictive-offer-completions-keylist
	'("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
)


;; construct the offer-completions keymap from the offer-completions-keylist
(let ((map (make-sparse-keymap)) key)
  (dotimes (i (length predictive-offer-completions-keylist))
    (setq key (nth i predictive-offer-completions-keylist))
    (define-key map key 'predictive-select-completion))
  (setq predictive-offer-completions-map map)
)



;; Set the syntax-alist that associates syntax with completion function, unless
;; it's been set already (most likely in an init file)
(unless predictive-syntax-alist
  (setq predictive-syntax-alist (list
    ;; word constituents add to current completion
    (cons ?w 'predictive-insert-and-complete)
    (cons ?_ 'predictive-insert-and-complete)
    ;; whitespace and punctuation chars accept current completion
    (cons ?  'predictive-accept-and-insert)
    (cons ?. 'predictive-accept-and-insert)
    ;; anything else abandons the current completion
    (cons t  'predictive-abandon-and-insert)))
)



;; Set the major-mode-alist so that things are set up sensibly in various
;; major modes, if it hasn't been set already (most likely in an init file)
(unless predictive-major-mode-alist
  (setq predictive-major-mode-alist
	'((text-mode  predictive-setup-english)
;;	  (latex-mode predictive-setup-latex)
;;	  (LaTeX-mode predictive-setup-latex)
;;	  (c-mode     predictive-setup-c)
	  ))
)




;; make sure `predictive-completing-map' is in `minor-mode-keymap-alist'
(let ((existing (assq 'predictive-completion-num minor-mode-map-alist)))
  (if existing
      (setcdr existing predictive-completing-map)
    (add-to-list
     'minor-mode-map-alist
     (cons 'predictive-completion-num predictive-completing-map)))
)



;; make sure `predictive-offer-completions-map' is in
;; `minor-mode-keymap-alist'
(let ((existing (assq 'predictive-completions-on-offer minor-mode-map-alist)))
  (if existing
      (setcdr existing predictive-offer-completions-map)
    (add-to-list
     'minor-mode-map-alist
     (cons 'predictive-completions-on-offer
	   predictive-offer-completions-map)))
)


;;; predictive.el ends here
