;;; predictive-latex.el --- predictive mode LaTeX setup function
;;;                         (assumes AMSmath)


;; Copyright (C) 2004-2012 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.12.12
;; Keywords: predictive, setup function, latex
;; URL: http://www.dr-qubit.org/emacs.php

;; This file is NOT part of Emacs.
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile (require 'cl))
(require 'predictive)
(require 'auto-overlay-word)
(require 'auto-overlay-line)
(require 'auto-overlay-self)
(require 'auto-overlay-nested)
(require 'predictive-auto-overlay-auto-dict)

;; compatibility defun
(unless (fboundp 'oddp)
  (defun oddp (i) (and (integerp i) (= (mod i 2) 1))))

;; prevent bogus compiler warnings
(eval-when-compile
  (defvar dict-latex-docclass)
  (defvar dict-latex-bibstyle)
  (defvar TeX-master))




;;;============================================================
;;;                  Customization Options

(defgroup predictive-latex nil
  "Predictive completion mode LaTeX support."
  :group 'predictive)


(defcustom predictive-latex-docclass-alist nil
  "Alist associating LaTeX document classes with dictionaries.
When a document class is in the list, "
  :group 'predictive-latex
  :type '(repeat (cons string symbol)))


(defcustom predictive-latex-save-section-dict nil
  "When non-nil, save the LaTeX section dictionary.

Disabled by default because saving the section dictionary has a
tendency to hit an irritating internal Emacs limit on printing
deeply nested structures, hard-coded in \"print.c\". Since the
section dictionary is only used for navigation, there is little
disadvantage in leaving this disabled.

Do *not* enable this without first applying the \"print.c.diff\"
patch (included in the Predictive Completion package) to the file
\"print.c\" in the Emacs source, and recompiling Emacs from the
patched source."
  :group 'predictive-latex
  :type 'boolean)


(defcustom predictive-latex-save-label-dict nil
  "When non-nil, save the LaTeX label dictionary.

Disabled by default because saving the label dictionaries has a
tendency to occasionally hit an irritating internal Emacs limit
on printing deeply nested structures, hard-coded in
\"print.c\". Not saving the label dictionary means all learnt
word weights for LaTeX label names are lost when a label
dictionary is unloaded.

If you only use short label names, it is reasonably safe to
enable this. However, if you see \"Apparently circular structure
being printed\" errors, then you must either disable this option,
or (better), apply the \"print.c.diff\" patch (included in the
Predictive Completion package) to the file \"print.c\" in the
Emacs source, and recompile Emacs from the patched source."
  :group 'predictive-latex
  :type 'boolean)


(defcustom predictive-latex-electric-environments nil
  "When enabled, environment names are automatically synchronized
between \\begin{...} and \\end{...} commands."
  :group 'predictive-latex
  :type 'boolean)




;;;============================================================
;;;                        Keymap

;; variable used to enable `predictive-latex-map' minor-mode keymap
;; (we don't use `define-minor-mode' because we explicitly don't want an
;; interactive minor-mode toggling command)
(defvar predictive-latex-mode nil)
(make-variable-buffer-local 'predictive-latex-mode)

;; keymap used for latex-specific `predictive-mode' key bindings
(defvar predictive-latex-map (make-sparse-keymap))

(push (cons 'predictive-latex-mode predictive-latex-map)
      minor-mode-map-alist)

;; override AUCTeX bindings so completion works
(define-key predictive-latex-map [?$]  'completion-self-insert)
(define-key predictive-latex-map [?\"] 'completion-self-insert)
(define-key predictive-latex-map [?_]  'completion-self-insert)
(define-key predictive-latex-map [?^]  'completion-self-insert)
(define-key predictive-latex-map [?\\] 'completion-self-insert)
(define-key predictive-latex-map [?-]  'completion-self-insert)




;;;============================================================
;;;                     Other variables

(defvar predictive-latex-usepackage-functions nil
  "List of LaTeX package functions.
Each entry should be a list of three elements, the first being
the package name (a string), the next two being the functions to
call when loading and unloading the package.")


(defvar predictive-latex-browser-submenu-alist nil
  "Alist associating regexps with sub-menu definitions.
When a browser menu item matches a regexp in the alist, the
associated definition is used to construct a sub-menu for that
browser item.

The sub-menu definition can be a function, symbol, dictionary, or
list of strings.

If it is a function, that function is called with two arguments:
the prefix being completed, and the menu item in question.  Its
return value should be a symbol, dictionary, or list of strings
to use as the sub-menu defition.

If the sub-menu definition is a symbol, the result of evaluating
the symbol should be a dictionary or list of strings, and is used
as the sub-menu definition.

If the sub-menu definition is a dictionary, the sub-menu is built
by surrounding every word in the dictionary with \"{\" \"}\", and
appending this to the end of the original item.

Finally, if the sub-menu definition is a list of strings, those
strings become the sub-menu entries.")

(make-variable-buffer-local 'predictive-latex-browser-submenu-alist)


(defvar predictive-latex-math-environments nil
  "List of LaTeX math environments.

Used by `predictive-latex-math-environment' to determine whether
within a LaTeX math environment.")

(make-variable-buffer-local 'predictive-latex-math-environments)


;; set up 'predictive-latex-word to be a `thing-at-point' symbol
(put 'predictive-latex-word 'forward-op 'predictive-latex-forward-word)
;; set up 'predictive-latex-label-word to be a `thing-at-point' symbol
(put 'predictive-latex-label-word 'forward-op
     'predictive-latex-label-forward-word)


;; variables holding dictionaries for different LaTeX contexts
(defvar predictive-latex-dict '(dict-latex))
(make-variable-buffer-local 'predictive-latex-dict)
(defvar predictive-latex-math-dict '(dict-latex-math))
(make-variable-buffer-local 'predictive-latex-math-dict)
(defvar predictive-latex-preamble-dict '(dict-latex-preamble))
(make-variable-buffer-local 'predictive-latex-preamble-dict)
(defvar predictive-latex-env-dict '(dict-latex-env))
(make-variable-buffer-local 'predictive-latex-env-dict)

(defvar predictive-latex-label-dict nil)
(make-variable-buffer-local 'predictive-latex-label-dict)
(defvar predictive-latex-local-latex-dict nil)
(make-variable-buffer-local 'predictive-latex-local-latex-dict)
(defvar predictive-latex-local-math-dict nil)
(make-variable-buffer-local 'predictive-latex-local-math-dict)
(defvar predictive-latex-local-env-dict nil)
(make-variable-buffer-local 'predictive-latex-local-env-dict)
(defvar predictive-latex-section-dict nil)
(make-variable-buffer-local 'predictive-latex-section-dict)


;; variables used to store `auto-completion-syntax-alist' completion,
;; punctuation and whitespace behaviours
(defvar predictive-latex-punctuation-resolve-behaviour nil)
(make-variable-buffer-local 'predictive-latex-punctuation-resolve-behaviour)
(defvar predictive-latex-word-completion-behaviour nil)
(make-variable-buffer-local 'predictive-latex-word-completion-behaviour)
(defvar predictive-latex-whitespace-resolve-behaviour nil)
(make-variable-buffer-local 'predictive-latex-whitespace-resolve-behaviour)


;; variable storing filename before saving, to detect renaming
;; (see `predictive-latex-after-save')
(defvar predictive-latex-previous-filename nil)
(make-variable-buffer-local 'predictive-latex-previous-filename)


;; convenience const holding alist associating latex dictionary variables and
;; corresponding dictionary name prefixes
(defconst predictive-latex-dict-classes
  '((predictive-latex-dict . "dict-latex-")
    (predictive-latex-math-dict . "dict-latex-math-")
    (predictive-latex-preamble-dict . "dict-latex-preamble-")
    (predictive-latex-env-dict . "dict-latex-env-")))


;; convenience consts defining useful regexp patterns
(defconst predictive-latex-odd-backslash-regexp
  "\\(?:[^\\]\\|^\\)\\(?:\\\\\\\\\\)*\\\\")

(defconst predictive-latex-brace-group-regexp
  "{\\(\\(?:\\w\\|\\s_\\|\\s.\\)*\\)")




;;;=========================================================
;;;           Register LaTeX Completion-UI sources

(completion-ui-register-derived-source
 predictive-latex predictive
 :override-syntax-alist
     ;; the \ character starts a LaTeX command unless it is preceded by an odd
     ;; number of \'s, in which case it is part of a \\ command
     ((?\\ predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-word-completion-behaviour)
      (?$ predictive-latex-punctuation-resolve-behaviour none)
      ;; typing a '{' character immediately triggers a new completion process
      ;; in some contexts
      (?{ predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-smart-open-brace-completion-behaviour)
      ;; typing a '"' character does smart quote insertion
      (?\" predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour
	   predictive-latex-smart-quote-insert-behaviour)
      ;; various characters form part of one-character LaTeX commands if
      ;; preceded by a '\'
      (?} predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?_ predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?^ predictive-latex-punctuation-resolve-behaviour none)
;;    (?' predictive-latex-single-char-command-resolve-behaviour
;;        predictive-latex-single-char-command-completion-behaviour)
      (?\( predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour)
      (?\) predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour)
      (?# predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?@ predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?+ predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?, predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?- predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?\; predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour)
      (?! predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?< predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?= predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?> predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      (?\[ predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour)
      (?\] predictive-latex-single-char-command-resolve-behaviour
	   predictive-latex-single-char-command-completion-behaviour)
      (?` predictive-latex-single-char-command-resolve-behaviour
	  predictive-latex-single-char-command-completion-behaviour)
      )
 :word-thing predictive-latex-word
 :menu (lambda (overlay)
	 (if (eq (aref (overlay-get overlay 'prefix) 0) ?\\)
	     (predictive-latex-construct-browser-menu overlay)
	   (completion-construct-menu overlay)))
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-derived-source
 predictive-latex-math predictive-latex
 :completion-function predictive-complete
 :completion-args 2
 :other-args (predictive-latex-math-dict)
 ;; :override-syntax-alist
 ;;     ((?' . (predictive-latex-punctuation-resolve-behaviour none t))
 ;;      (?$ predictive-latex-punctuation-resolve-behaviour none))
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-derived-source
 predictive-latex-preamble predictive
 :completion-function predictive-complete
 :completion-args 2
 :other-args (predictive-latex-preamble-dict)
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-source
 predictive-complete
 :name predictive-latex-env
 :completion-args 2
 :other-args (predictive-latex-env-dict)
 :accept-functions (lambda (prefix completion &optional arg)
		     (when predictive-latex-electric-environments
		       (predictive-latex-update-matching-env completion))
		     (run-hook-with-args 'predictive-accept-functions
					 prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-reject-functions
					 prefix completion arg))
 :syntax-alist
     ((?w . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?_ . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour t))
      (?. . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour t))
      (?  . (predictive-latex-whitespace-resolve-behaviour none))
      (t  . (reject none)))
 :override-syntax-alist
     ((?} predictive-latex-punctuation-resolve-behaviour none))
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-function
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-source
 predictive-complete
 :name predictive-latex-label
 :completion-args 2
 :other-args (predictive-latex-label-dict)
 :accept-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-accept-functions
					 prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-reject-functions
					 prefix completion arg))
 :syntax-alist
     ((?w . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?_ . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?. . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?  . (predictive-latex-whitespace-resolve-behaviour none))
      (t  . (reject none)))
 :override-syntax-alist
     ((?: . ((lambda () (predictive-latex-completion-add-till-regexp ":"))
	     predictive-latex-word-completion-behaviour))
      (?_ . ((lambda () (predictive-latex-completion-add-till-regexp "\\W"))
	     predictive-latex-word-completion-behaviour))
      (?} . (predictive-latex-punctuation-resolve-behaviour none)))
 :word-thing predictive-latex-label-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-source
 predictive-complete
 :name predictive-latex-docclass
 :completion-args 2
 :other-args (dict-latex-docclass)
 :accept-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-accept-functions
					 prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-reject-functions
					 prefix completion arg))
 :syntax-alist
     ((?w . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?_ . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?. . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?  . (predictive-latex-whitespace-resolve-behaviour none))
      (t  . (reject none)))
 :override-syntax-alist
     ((?} predictive-latex-punctuation-resolve-behaviour none))
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(completion-ui-register-source
 predictive-complete
 :name predictive-latex-bibstyle
 :completion-args 2
 :other-args (dict-latex-bibstyle)
 :accept-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-accept-functions
					 prefix completion arg))
 :reject-functions (lambda (prefix completion &optional arg)
		     (run-hook-with-args 'predictive-reject-functions
					 prefix completion arg))
 :syntax-alist
     ((?w . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?_ . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?. . (predictive-latex-smart-within-braces-resolve-behaviour
	     predictive-latex-word-completion-behaviour))
      (?  . (predictive-latex-whitespace-resolve-behaviour none))
      (t  . (reject none)))
 :override-syntax-alist
     ((?} predictive-latex-punctuation-resolve-behaviour none))
 :word-thing predictive-latex-word
 :menu predictive-latex-construct-browser-menu
 :browser predictive-latex-construct-browser-menu
 :no-auto-completion t
 :no-command t
 :no-predictive t)


(defun predictive-latex-math-environment ()
  ;; Return 'predictive-latex-math auto-completion source if within a LaTeX
  ;; math environment. Locally added to `auto-completion-source-functions' by
  ;; `predictive-setup-latex'.
  (when (member (LaTeX-current-environment)
		predictive-latex-math-environments)
    'predictive-latex-math))




;;;=========================================================
;;;                  The setup function

(defun predictive-setup-latex (arg)
  "With a positive ARG, set up predictive mode for LaTeX.
With a negative ARG, undo these changes.

The default value of `predictive-major-mode-alist' calls this
function automatically when predictive mode is enabled in
`latex-mode' or `LaTeX-mode'."

  (cond
   ;; ----- enabling LaTeX setup -----
   ((> arg 0)
    (catch 'load-fail
      ;; make predictive completion the default auto-completion source
      (predictive-setup-save-local-state 'auto-completion-default-source)
      (set (make-local-variable 'auto-completion-default-source)
	   'predictive-latex)
      ;; enable `predictive-latex-map' keymap
      (setq predictive-latex-mode t)
      ;; save overlays and dictionaries along with buffer
      ;;(add-hook 'after-save-hook 'predictive-latex-after-save nil 'local)
      (add-hook 'kill-buffer-hook 'predictive-latex-kill-buffer nil 'local)
      ;; store filename for comparison when saving (see
      ;; `predictive-latex-after-save')
      (predictive-setup-save-local-state 'predictive-latex-previous-filename)
      (setq predictive-latex-previous-filename (buffer-file-name))
      ;; set custom list of functions for determining auto-completion source
      (predictive-setup-save-local-state 'auto-completion-source-functions)
      (set (make-local-variable 'auto-completion-source-functions)
	   '(auto-completion-regexp-source
	     auto-completion-face-source
	     predictive-latex-math-environment))
      ;; configure syntax-related settings
      (predictive-latex-load-syntax)
      ;; consider \ as start of a word
      (predictive-setup-save-local-state 'words-include-excapes)
      (set (make-local-variable 'words-include-escapes) nil)
      ;; set initial list of LaTeX math environments
      (predictive-setup-save-local-state 'predictive-latex-math-environments)
      (setq predictive-latex-math-environments
	    '("equation" "displaymath" "equation*" "align" "align*"
	      "gather" "gather*" "multline" "multine*" "alignat" "alignat*"
	      "flalign" "flalign*"))
      ;; set initial browser submenu definitions
      (predictive-setup-save-local-state
       'predictive-latex-browser-submenu-alist)
      (setq predictive-latex-browser-submenu-alist
	   '(("\\\\begin" . dict-latex-env)
	     ("\\\\documentclass" . dict-latex-docclass)
	     ("\\\\bibliographystyle" . dict-latex-bibstyle)
	     ("\\\\\\(eq\\|\\)ref" . predictive-latex-label-dict)))

      (cond
       ;; If we're not the `TeX-master', visit the TeX master buffer, enable
       ;; predictive mode in it, and share buffer-local settings with it.
       ((and (boundp 'TeX-master) (stringp TeX-master))
	(predictive-latex-inherit-from-TeX-master)
	;; start the auto-overlays
	;; Note: we skip the check that regexp definitions haven't changed
	;;       if there's a file of saved overlay data to use, since
	;;       definitions won't match if packages load additional regexps
;;      (let ((restore-modified (buffer-modified-p)))
	(auto-overlay-start 'predictive nil
			    predictive-local-auxiliary-file-directory
			    'no-regexp-check)
;;	  (set-buffer-modified-p restore-modified))
	)

       ;; If we're the `TeX-master', set up LaTeX auto-completion source and
       ;; auto-overlay regexps.
       ;; FIXME: should we handle null TeX-master case differently?
       (t
	(predictive-latex-load-dicts)
	(predictive-latex-load-regexps)
	;; load latex auto-overlay regexps
	(auto-overlay-unload-set 'predictive)
	(predictive-latex-load-auto-overlay-definitions)
	;; Note: we skip the check that regexp definitions haven't changed if
	;;       there's a file of saved overlay data to use, since
	;;       definitions won't match if packages load additional regexps
;;	(let ((restore-modified (buffer-modified-p)))
	(auto-overlay-start 'predictive nil
			    predictive-local-auxiliary-file-directory
			    'no-regexp-check)
;;	  (set-buffer-modified-p restore-modified))
	))
      t))  ; indicate successful setup


   ;; ----- Disabling LaTeX setup -----
   ((< arg 0)
    ;; disable `predictive-latex-map' keymap
    (setq predictive-latex-mode nil)
    ;; if we're the `TeX-master', first disable predictive mode in all related
    ;; LaTeX buffers, which we find by looking for buffers that share the
    ;; 'predictive auto-overlays regexp set
    (unless (and (boundp 'TeX-master) (stringp TeX-master))
      (dolist (buff (auto-o-get-buffer-list 'predictive))
	;; TeX-master itself will be in list of buffers sharing regexp set, so
	;; need to filter it out
	(unless (eq buff (current-buffer))
	  (with-current-buffer buff (predictive-mode -1)))))
    ;; stop predictive auto overlays
    (auto-overlay-stop 'predictive nil
		       (when (buffer-file-name)
			 predictive-local-auxiliary-file-directory))
    (auto-overlay-unload-set 'predictive)
    ;; unload local dicts, without saving if buffer wasn't saved
    (unless (and (boundp 'TeX-master) (stringp TeX-master))
      (predictive-latex-unload-dicts))
    ;; remove hook functions that save overlays etc.
    ;;(remove-hook 'after-save-hook 'predictive-latex-after-save 'local)
    (remove-hook 'kill-buffer-hook 'predictive-latex-kill-buffer 'local)
    ;; restore saved state
    (predictive-setup-restore-local-state)
    t)))  ; indicate successful disabling



(defun predictive-latex-load-dicts ()
  ;; load the predictive-mode LaTeX dictionaries
  (mapc (lambda (dic)
	  (unless (predictive-load-dict dic)
	    (message "Failed to load %s" dic)
	    (throw 'load-fail nil)))
	(append predictive-latex-dict
		predictive-latex-math-dict
		predictive-latex-preamble-dict
		predictive-latex-env-dict
		(list 'dict-latex-bibstyle)
		(list 'dict-latex-docclass)))

  (predictive-setup-save-local-state
   'predictive-latex-label-dict
   'predictive-latex-local-latex-dict
   'predictive-latex-local-math-dict
   'predictive-latex-local-env-dict
   'predictive-latex-section-dict)
  ;; load/create the local latex and label dictionaries
  (setq predictive-latex-label-dict
	  (predictive-auto-dict-load "latex-label")
	predictive-latex-local-latex-dict
	  (predictive-auto-dict-load "latex-local-latex")
	predictive-latex-local-math-dict
	  (predictive-auto-dict-load "latex-local-math")
	predictive-latex-local-env-dict
	  (predictive-auto-dict-load "latex-local-env")
	predictive-latex-section-dict
	  (predictive-auto-dict-load "latex-section"))

  ;; disable saving of section and label dictionaries to avoid Emacs bug
  (unless predictive-latex-save-section-dict
    (setf (dictree-autosave predictive-latex-section-dict) nil))
  (unless predictive-latex-save-label-dict
    (setf (dictree-autosave predictive-latex-label-dict) nil))

  ;; add local env, maths and text-mode dicts to appropriate dict lists
  ;; Note: we add the local text-mode command dictionary to the maths
  ;;       dictionary list too, because there's no way to tell whether
  ;;       \newcommand's are text- or math-mode commands.
  (setq predictive-latex-dict
	(append predictive-latex-dict
		(if (dictree-p predictive-latex-local-latex-dict)
		    (list predictive-latex-local-latex-dict)
		  predictive-latex-local-latex-dict)))
  (setq predictive-latex-math-dict
	(append predictive-latex-math-dict
		(if (dictree-p predictive-latex-local-math-dict)
		    (list predictive-latex-local-math-dict)
		  predictive-latex-local-math-dict)
		(if (dictree-p predictive-latex-local-latex-dict)
		    (list predictive-latex-local-latex-dict)
		  predictive-latex-local-latex-dict)))
  (setq predictive-latex-env-dict
	(append predictive-latex-env-dict
		(if (dictree-p predictive-latex-local-env-dict)
		    (list predictive-latex-local-env-dict)
		  predictive-latex-local-env-dict)))

  ;; set latex dictionaries to be used alongside main dictionaries
  (setq predictive-auxiliary-dict predictive-latex-dict))


(defun predictive-latex-unload-dicts ()
  ;; unload the predictive-mode LaTeX dictionaries
  (predictive-auto-dict-unload "latex-label" nil (buffer-modified-p))
  (predictive-auto-dict-unload "latex-local-latex" nil (buffer-modified-p))
  (predictive-auto-dict-unload "latex-local-math" nil (buffer-modified-p))
  (predictive-auto-dict-unload "latex-local-env" nil (buffer-modified-p))
  (predictive-auto-dict-unload "latex-section" nil (buffer-modified-p)))



(defun predictive-latex-load-regexps ()
  ;; load the predictive-mode `auto-completion-source-regexps'
  ;; configure automatic selection of completion sources and dicts
  (predictive-setup-save-local-state 'auto-completion-source-faces)
  (set (make-local-variable 'auto-completion-source-faces)
       '(nil  ; allows non-TeX-master buffers to share face defs
	 (font-latex-math-face . predictive-latex-math)))
  (predictive-setup-save-local-state 'auto-completion-source-regexps)
  (set (make-local-variable 'auto-completion-source-regexps)
       `(nil  ; allows non-TeX-master buffers to share regexp defs
	 (,(concat predictive-latex-odd-backslash-regexp
		   "label" predictive-latex-brace-group-regexp)
	  nil looking-at 1)
	 (,(concat predictive-latex-odd-backslash-regexp
		   "\\(?:begin\\|end\\)"
		   predictive-latex-brace-group-regexp)
	  predictive-latex-env looking-at 1)
	 (,(concat predictive-latex-odd-backslash-regexp
		   "ref" predictive-latex-brace-group-regexp)
	  predictive-latex-label looking-at 1)
	 (,(concat predictive-latex-odd-backslash-regexp
		   "documentclass\\(?:\\[.*\\]\\)?"
		   predictive-latex-brace-group-regexp)
	  predictive-latex-docclass looking-at 1)
	 (,(concat predictive-latex-odd-backslash-regexp
		   "bibliographystyle\\(?:\\[.*\\]\\)?"
		   predictive-latex-brace-group-regexp)
	  predictive-latex-bibstyle looking-at 1)
	 (,(concat predictive-latex-odd-backslash-regexp
		   "text{\\([^}]*\\)")
	  predictive looking-at 1))))



(defun predictive-latex-load-auto-overlay-definitions ()
  "Load the predictive mode LaTeX auto-overlay regexp definitions."

  ;; %'s start comments that last till end of line
  (auto-overlay-load-definition
   'predictive
   `(line :id comment
	  ("%" (dict . predictive-main-dict)
	   (priority . 100) (exclusive . t))))

  ;; preamble lives between \documentclass{...} and \begin{document}
  (auto-overlay-load-definition
   'predictive
   `(nested :id preamble
	    (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\documentclass\\(\\[.*?\\]\\)?{.*?}\\)" . 3)
	     :edge start
	     (completion-source . predictive-latex-preamble)
	     (priority . 20))
	    (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\(\\\\begin{document}\\)" . 3)
	     :edge end
	     (completion-source . predictive-latex-preamble)
	     (priority . 20))
	    ))

  ;; \documentclass defines the document type. Through the use of a special
  ;; "docclass" regexp class defined below, this automagically changes the
  ;; main dictionary if one is defined for the docclass in
  ;; `predictive-latex-docclass-alist'
  (auto-overlay-load-definition
   'predictive
   '(predictive-latex-docclass
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\documentclass\\(\\[.*?\\]\\)?{\\(.*?\\)}" . 4))))

  ;; \usepackage loads a latex package. Through the use of a special
  ;; "usepackage" regexp class defined below, this automagically loads new
  ;; dictionaries and auto-overlay regexps.
  (auto-overlay-load-definition
   'predictive
   '(predictive-latex-usepackage
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\usepackage\\(\\[.*?\\]\\)?{\\(.*?\\)}"
       . 4))))

  ;; \label creates a cross-reference label. Through the use of a special
  ;; "auto-dict" regexp class defined below, this automagically adds the label
  ;; to the label dictionary.
  (auto-overlay-load-definition
   'predictive
   '(predictive-auto-dict
     :id label
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\label{\\(.*?\\)}" . 3)
      (auto-dict . predictive-latex-label-dict))))

  ;; \newcommand defines a new command. Through the use of a special
  ;; "auto-dict" regexp class defined below, this automagically adds the
  ;; command to the LaTeX and math dictionaries (there's no way to tell
  ;; whether the new command is a text-mode or math-mode command, so we add it
  ;; to both).
  (auto-overlay-load-definition
   'predictive
   '(predictive-auto-dict
     :id newcommand
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newcommand\\*?{\\(.*?\\)}" . 3)
      (auto-dict . predictive-latex-local-latex-dict))))

  ;; \newenvironment and \newtheorem define new environments. Through the use
  ;; of a special "auto-dict" regexp class defined below, this automagically
  ;; adds the environment to the local environment dictionary.
  (auto-overlay-load-definition
   'predictive
   '(predictive-auto-dict
     :id newenvironment
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newenvironment{\\(.*?\\)}" . 3)
      (auto-dict . predictive-latex-local-env-dict))))
  (auto-overlay-load-definition
   'predictive
   '(predictive-auto-dict
     :id newtheorem
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\newtheorem{\\(.*?\\)}" . 3)
      (auto-dict . predictive-latex-local-env-dict))))

  ;; \DeclareMathOperator defines a new math-mode command. Through the use of
  ;; a special "auto-dict" regexp class defined below, this automagically adds
  ;; the environment to the local maths dictionary.
  (auto-overlay-load-definition
   'predictive
   '(predictive-auto-dict
     :id DeclareMathOperator
     (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\DeclareMathOperator\\*?{\\(.*?\\)}"
       . 3)
      (auto-dict . predictive-latex-local-math-dict))))

  ;; ;; the sectioning commands automagically add the section names to a local
  ;; ;; sections dictionary, purely for navigation
  ;; (auto-overlay-load-definition
  ;;  'predictive
  ;;  '(predictive-auto-dict
  ;;    :id section
  ;;    (("\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\\\\\\(\\(sub\\)\\{,2\\}section\\*?\\|chapter\\){\\(.*?\\)}" . 5)
  ;; 	(auto-dict . predictive-latex-section-dict))))
  )


(defun predictive-latex-load-syntax ()
  "Load the predictive mode LaTeX Completion-UI syntax behaviour."
  ;; get behaviours defined in `auto-completion-syntax-alist'
  (destructuring-bind (word-resolve word-complete word-insert
		       punct-resolve punct-complete punct-insert
		       whitesp-resolve whitesp-complete whitesp-insert)
      (append (auto-completion-lookup-behaviour nil ?w 'predictive)
	      (auto-completion-lookup-behaviour nil ?. 'predictive)
	      (auto-completion-lookup-behaviour nil ?  'predictive))
    ;; store behaviours in variables for source syntax-alists
    (setq predictive-latex-word-completion-behaviour word-complete)
    (setq predictive-latex-punctuation-resolve-behaviour punct-resolve)
    (setq predictive-latex-whitespace-resolve-behaviour whitesp-resolve)))



(defun predictive-latex-inherit-from-TeX-master ()
  ;; inherit predictive-latex settings from `TeX-master'
  (let (filename buff source-regexps source-faces
		 used-dicts main-dict aux-dict
		 latex-dict math-dict preamble-dict env-dict
		 label-dict local-latex-dict local-math-dict
		 local-env-dict local-section-dict
		 browser-submenu)
    (setq filename (concat (file-name-sans-extension
			    (expand-file-name TeX-master)) ".tex"))
    (unless (file-exists-p filename) (throw 'load-fail nil))
    (save-window-excursion
      (find-file filename)
      (turn-on-predictive-mode)
      (setq buff (current-buffer)
	    used-dicts         predictive-used-dict-list
	    source-regexps     auto-completion-source-regexps
	    source-faces       auto-completion-source-faces
	    main-dict          predictive-buffer-dict
	    aux-dict           predictive-auxiliary-dict
	    latex-dict         predictive-latex-dict
	    math-dict          predictive-latex-math-dict
	    preamble-dict      predictive-latex-preamble-dict
	    env-dict           predictive-latex-env-dict
	    label-dict         predictive-latex-label-dict
	    local-latex-dict   predictive-latex-local-latex-dict
	    local-math-dict    predictive-latex-local-math-dict
	    local-env-dict     predictive-latex-local-env-dict
	    local-section-dict predictive-latex-section-dict
	    browser-submenu    predictive-latex-browser-submenu-alist))
    (auto-overlay-share-regexp-set 'predictive buff)

    (predictive-setup-save-local-state
     'auto-completion-source-regexps
     'auto-completion-source-faces)
    (set (make-local-variable 'auto-completion-source-regexps)
	 source-regexps)
    (set (make-local-variable 'auto-completion-source-faces)
	 source-faces)

    (predictive-setup-save-local-state
     'predictive-used-dict-list
     'predictive-buffer-dict
     'predictive-auxiliary-dict
     'predictive-latex-dict
     'predictive-latex-math-dict
     'predictive-latex-preamble-dict
     'predictive-latex-env-dict
     'predictive-latex-label-dict
     'predictive-latex-local-latex-dict
     'predictive-latex-local-math-dict
     'predictive-latex-local-env-dict
     'predictive-latex-section-dict
     'predictive-latex-browser-submenu-alist)
    (setq predictive-used-dict-list         used-dicts
	  predictive-buffer-dict            main-dict
	  predictive-auxiliary-dict         aux-dict
	  predictive-latex-dict             latex-dict
	  predictive-latex-math-dict        math-dict
	  predictive-latex-preamble-dict    preamble-dict
	  predictive-latex-env-dict         env-dict
	  predictive-latex-label-dict       label-dict
	  predictive-latex-local-latex-dict local-latex-dict
	  predictive-latex-local-math-dict local-math-dict
	  predictive-latex-local-env-dict   local-env-dict
	  predictive-latex-section-dict     local-section-dict
	  predictive-latex-browser-submenu-alist browser-submenu)))



(defun predictive-latex-kill-buffer ()
  ;; Function called from `kill-buffer-hook' to tidy things up
  ;; save overlays if buffer was saved
  (unless (buffer-modified-p)
    (when (buffer-file-name)
      (auto-overlay-save-overlays
       'predictive nil predictive-local-auxiliary-file-directory))
    ;; if we're not the TeX-master, unload the regexps to unshare them
    (if (and (boundp 'TeX-master) (stringp TeX-master))
	(auto-overlay-unload-set 'predictive)
      ;; if we're the TeX master, first disable predictive mode in all related
      ;; LaTeX buffers, which we find by looking for buffers that share the
      ;; `predictive' auto-overlays regexp set
      (dolist (buff (auto-o-get-buffer-list 'predictive))
	;; TeX-master itself will be in list of buffers sharing regexp set, so
	;; need to filter it out; the test for null buffer name avoids deleted
	;; buffers, though this should never occur.
	(unless (or (eq buff (current-buffer)) (null (buffer-name buff)))
	  (with-current-buffer buff (predictive-mode -1))))
      ;; unload local dicts, saving if buffer is saved
      (predictive-auto-dict-unload "latex-label" nil (buffer-modified-p))
      (predictive-auto-dict-unload "latex-local-latex" nil (buffer-modified-p))
      (predictive-auto-dict-unload "latex-local-math" nil (buffer-modified-p))
      (predictive-auto-dict-unload "latex-local-env" nil (buffer-modified-p))
      (predictive-auto-dict-unload "latex-section" nil (buffer-modified-p)))))


(defun predictive-latex-after-save ()
  ;; Function called from `after-save-hook'
  (auto-overlay-save-overlays 'predictive nil
			      predictive-local-auxiliary-file-directory)
  ;; if file has not been renamed, just save local dicts
  (if (or (and (null predictive-latex-previous-filename)
	       (null (buffer-file-name)))
	  (string= predictive-latex-previous-filename
		   (buffer-file-name)))
      (unless (and (boundp 'TeX-master) (stringp TeX-master))
	(predictive-auto-dict-save "latex-local-latex")
	(predictive-auto-dict-save "latex-local-math")
	(predictive-auto-dict-save "latex-local-env")
	(when predictive-latex-save-label-dict
	  (predictive-auto-dict-save "latex-label"))
	(when predictive-latex-save-section-dict
	  (predictive-auto-dict-save "latex-section")))
    ;; otherwise, restart predictive-mode to set everything up afresh
    (let ((restore (buffer-file-name)))
      (set-visited-file-name predictive-latex-previous-filename)
      (predictive-mode -1)
      (set-visited-file-name restore)
      (set-buffer-modified-p nil)
      (predictive-mode 1)))

  ;; store visited file name for comparison next time buffer is saved
  (setq predictive-latex-previous-filename (buffer-file-name))
  ;; repeat file save nessage (overwritten by overlay and dict save messages)
  (message "Wrote %s and saved predictive-mode state" (buffer-file-name)))




;;;=======================================================================
;;;                  Miscelaneous interactive commands

(defun predictive-latex-reparse-buffer ()
  "Reparse a LaTeX buffer from scratch."
  (interactive)
  (predictive-mode -1)
  ;; using an internale auto-overlay function is ugly, but then this command
  ;; shouldn't be necessary anyway!
  (delete-file (concat predictive-local-auxiliary-file-directory
		       (auto-o-overlay-filename 'predictive)))
  (predictive-mode 1))


(defun predictive-latex-update-matching-env (&optional env)
  "Update matching LaTeX \begin or \end to match the one at point."
  (interactive)
  (unless env (setq env (thing-at-point 'predictive-latex-word)))

  (let ((pos (point)) match)
    (save-excursion
      (cond
       ;; at \begin, find matching \end
       ((and (or (end-of-line) t)
	     (re-search-backward "\\\\begin{\\([[:alnum:]]*\\)}"
				 (line-beginning-position) t)
	     (>= pos (match-beginning 1))
	     (<= pos (match-end 1)))
	(goto-char pos)
	(when (and (setq pos (condition-case nil
				 (LaTeX-find-matching-end)
			       (error nil)))
		   (re-search-backward "\\\\end{\\([[:alnum:]]*\\)}"
				       (line-beginning-position) t)
		   (= (match-end 0) pos))
	  (setq match (match-end 1))))
       ;; at \end, find matching \begin
       ((and (or (end-of-line) t)
	     (re-search-backward "\\\\end{\\([[:alnum:]]*\\)}"
				 (line-beginning-position) t)
	     (>= pos (match-beginning 1))
	     (<= pos (match-end 1)))
	(goto-char pos)
	(when (and (setq pos (condition-case nil
				 (LaTeX-find-matching-begin)
			       (error nil)))
		   (looking-at "\\\\begin{\\([[:alnum:]]*\\)}"))
	  (setq match (match-end 1)))))
      ;; update matching \begin or \end
      (when match
	(goto-char match)
	(delete-region (match-beginning 1) (match-end 1))
	(insert env)))))



;;;=======================================================================
;;;                       Jump commands

(defun predictive-latex-jump-to-definition ()
  "Jump to definition of whatever is at point.
\(Can be a label, or a command or environment defined in the
document's preamble\).

If point is already on a definition, cycle to next duplicate
definition of the same thing."
  (interactive)

  (let ((source (auto-completion-source))
	word dict type o-def)
    (or
     ;; when we're on either a cross-reference or a label definition...
     (and (completion-ui-source-derives-from-p source 'predictive-latex-label)
	  ;; look for label at point
	  (setq word
		(thing-at-point (completion-ui-source-word-thing source)))
	  (set-text-properties 0 (length word) nil word)
	  (setq dict predictive-latex-label-dict)
	  (setq type "label"))

     ;; when we're on either a LaTeX command or a definition thereof...
     (and (or (completion-ui-source-derives-from-p
	       source '(predictive-latex 'predictive-latex-math))
	      (setq o-def
		    (car (auto-overlays-at-point
			  nil `((identity auto-overlay)
				(eq set-id predictive)
				(,(lambda (id)
				    (or (eq id 'newcommand)
					(eq id 'DeclareMathOperator)))
				 definition-id))))))
	  ;; look for command at point
	  (setq word (thing-at-point 'predictive-latex-word))
	  (set-text-properties 0 (length word) nil word)
	  ;; verify we're on a command by checking first character is "\"
	  (= (elt word 0) ?\\)
	  ;; set dict to temporary meta-dict that combines local-latex and
	  ;; local-math dicts
	  (setq dict (dictree-create-meta-dict
		      (list predictive-latex-local-latex-dict
			    predictive-latex-local-math-dict)
		      nil nil nil t '+))
	  (setq type "command"))

     ;; when we're on either a LaTeX environment or definition thereof...
     (and (or (completion-ui-source-derives-from-p
	       source 'predictive-latex-env)
	      (setq o-def (car (auto-overlays-at-point
				nil `((identity auto-overlay)
				      (eq set-id predictive)
				      (,(lambda (id)
					  (or (eq id 'newenvironment)
					      (eq id 'newtheorem)))
				       definition-id))))))
	  ;; look for environment at point
	  (setq word (thing-at-point 'predictive-latex-word))
	  (set-text-properties 0 (length word) nil word)
	  (setq dict predictive-latex-local-env-dict)
	  (setq type "environment")))


    (if (null type)
	(message "Nothing to jump to")
      ;; jump to definition
      (setq o-def (predictive-auto-dict-jump-to-def dict word o-def))
      (cond
       ;; we only find out here whether a command or environment was defined
       ;; in preamble or globally, so we might have jumped no where
       ((null o-def) (message "Nothing to jump to"))
       ;; display warning if multiply defined
       ((> (length o-def) 1)
	(message "LaTeX %s \"%s\" multiply defined" type word))))
    ))



(defvar predictive-latex-label-history nil
  "History list for commands that read a LaTeX label.")

(defun predictive-latex-jump-to-label-definition (&optional label)
  "Jump to the definition of LABEL in the current LaTeX document.
If point is already on the definition of LABEL, jump to the next
duplicate definition of LABEL.

Interactively, LABEL is read from the mini-buffer, defaulting to
the label at point (if any)."
  (interactive)

  (let ((dict predictive-latex-label-dict)
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for cross-reference or label definition at point
    (unless label
      (when (or (member dict current-dict)
		(setq o-def (car (auto-overlays-at-point
				  nil '((identity auto-overlay)
					(eq set-id predictive)
					(eq definition-id label))))))
	(setq label
	      (thing-at-point
	       (let ((completion-word-thing 'predictive-latex-label-word))
		 (auto-overlay-local-binding 'completion-word-thing))))
	(when label (set-text-properties 0 (length label) nil label))))

    ;; interactively, read label from minibuffer, defaulting to what we've
    ;; found
    (when (called-interactively-p 'any)
      (let ((label-tmp
	     (completing-read
	      (if label
		  (format "LaTeX label (default \"%s\"): " label)
		"LaTeX label: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-latex-label-history label t)))
	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= label label-tmp)
	  (setq label label-tmp)
	  (setq o-def nil))))

    ;; jump to definition
    (unless (or (null label) (string= label ""))
      (setq o-def (predictive-auto-dict-jump-to-def dict label o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "LaTeX label \"%s\" multiply defined" label))
      t)  ; return t to indicate we jumped somehwere
    ))



(defvar predictive-latex-command-history nil
  "History list for commands that read a LaTeX command.")

(defun predictive-latex-jump-to-command-definition (&optional command)
  "Jump to the definition of COMMAND in the current LaTeX document.

Interactively, COMMAND is read from the mini-buffer, defaulting
to the command at point (if any). This only jumps to commands
that are defined in the document's preamble."
  (interactive)

  (let ((dict (dictree-create-meta-dict
	       (list predictive-latex-local-latex-dict
		     predictive-latex-local-math-dict)
	       nil nil nil t '+))
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for command name or definition at point
    (unless command
      (when (or (member
		 (symbol-value (predictive-auto-dict-name "latex-local-latex"))
		 current-dict)
		(member
		 (symbol-value (predictive-auto-dict-name "latex-local-math"))
		 current-dict)
		(setq o-def
		      (car (auto-overlays-at-point
			    nil `((identity auto-overlay)
				  (eq set-id predictive)
				  (,(lambda (id)
				      (or (eq id 'newcommand)
					  (eq id 'DeclareMathOperator)))
				   definition-id))))))
	(setq command (thing-at-point 'predictive-latex-word))
	(when command
	  (set-text-properties 0 (length command) nil command)
	  (unless (= (elt command 0) ?\\) (setq command nil)))))

    ;; interactively, read command from minibuffer, defaulting to what we've
    ;; found
    (when (called-interactively-p 'any)
      (let ((command-tmp
	     (completing-read
	      (if command
		  (format "LaTeX command (default \"%s\"): " command)
		"LaTeX command: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-latex-command-history command t)))
      	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= command command-tmp)
	  (setq command command-tmp)
	  (setq o-def nil))))

    ;; jump to definition
    (unless (or (null command) (string= command ""))
      (setq o-def (predictive-auto-dict-jump-to-def dict command o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "LaTeX command \"%s\" multiply defined" command))
      t)  ; return t to indicate we jumped somewhere
    ))



(defvar predictive-latex-environment-history nil
  "History list for commands that read a LaTeX environment.")

(defun predictive-latex-jump-to-environment-definition (&optional env)
  "Jump to the definition of ENV environment in the current LaTeX document.

Interactively, ENV is read from the mini-buffer, defaulting to
the environment at point (if any). This only jumps to
environments that are defined in the document's preamble."
  (interactive)

  (let ((dict predictive-latex-local-env-dict)
	(current-dict (predictive-current-dict))
	o-def)
    (when (dictree-p current-dict) (setq current-dict (list current-dict)))

    ;; look for environment name or defininition at point
    (unless env
      (when (or (member dict (predictive-current-dict))
		(setq o-def (car (auto-overlays-at-point
				  nil `((identity auto-overlay)
					(eq set-id predictive)
					(,(lambda (id)
					    (or (eq id 'newenvironment)
						(eq id 'newtheorem)))
					 definition-id))))))
	;; look for environment at point
	(setq env (thing-at-point 'predictive-latex-word))
	(when env (set-text-properties 0 (length env) nil env))))

    ;; interactively, read environment from minibuffer, defaulting to what
    ;; we've found
    (when (called-interactively-p 'any)
      (let ((env-tmp
	     (completing-read
	      (if env
		  (format "LaTeX environment (default \"%s\"): " env)
		"LaTeX environment: ")
	      (lambda (string predicate all)
		(dictree-collection-function dict string predicate all))
	      nil t nil 'predictive-latex-environment-history env t)))
	;; unless user accepted default, any overlay we found is no longer
	;; relevant
	(unless (string= env env-tmp)
	  (setq env env-tmp)
	  (setq o-def nil))))

    (unless (or (null env) (string= env ""))
      ;; jump to definition
      (setq o-def (predictive-auto-dict-jump-to-def dict env o-def))
      ;; display warning if multiply defined
      (when (> (length o-def) 1)
	(message "LaTeX environment \"%s\" multiply defined" env))
      t)  ; return t to indicate we jumped somewhere
    ))



(defvar predictive-latex-section-history nil
  "History list for commands that read a LaTeX section name.")

(defun predictive-latex-jump-to-section (&optional section)
  "Jump to the start of SECTION in the current LaTeX document.

Interactively, SECTION is read from the mini-buffer."
  (interactive)

  (let ((dict predictive-latex-section-dict))
    ;; interactively, read section name from minibuffer, defaulting to what
    ;; we've found
    (when (called-interactively-p 'any)
      (setq section
	    (completing-read
	     "Section: "
	     (lambda (string predicate all)
	       (dictree-collection-function dict string predicate all))
	     nil t nil 'predictive-latex-section-history nil t)))

    ;; jump to section
    (unless (or (null section) (string= section ""))
      (let ((o-def (predictive-auto-dict-jump-to-def dict section)))
	;; display warning if multiply defined
	(when (> (length o-def) 1)
	  (message "Multiple sections called \"%s\"" section))
	t))  ; return t to indicate we jumped somehwere
    ))



(defun predictive-latex-jump-to-matching-delimiter ()
  "Jump to LaTeX delimiter matching the one at point
\(\\begin{...} <-> \\end{...}, \\[ <-> \\], or $ <-> $\)."
  (interactive)

  ;; get innermost LaTeX environment match overlay
  (let ((o-match
	 (car (sort (auto-overlays-at-point
		     nil `((eq auto-overlay-match t)
			   (eq set-id predictive)
			   (,(lambda (definition-id)
			       (or (eq definition-id 'environment)
				   (eq definition-id 'inline-math)
				   (eq definition-id 'display-math)))
			    definition-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-parent o-other)

    ;; if we haven't found a match overlay, display a message
    (if (not o-match)
	(message "Not at a LaTeX delimiter")
      ;; otherwise, if other edge of its parent is not matched, display
      ;; message
      (if (not (and (setq o-parent (overlay-get o-match 'parent))
		    (setq o-other
			  (overlay-get o-parent
				       (if (eq o-match
					       (overlay-get o-parent 'start))
					   'end 'start)))))
	  (message "Unmatched LaTeX delimiter")
	;; otherwise, move point to the other edge
	(push-mark)
	(goto-char (overlay-get o-other
				(if (eq o-other (overlay-get o-parent 'start))
				    'delim-end 'delim-start)))))
    ))



(defun predictive-latex-jump-to-start-delimiter ()
  "Jump to start of LaTeX environment or equation at point."
  (interactive)

  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (definition-id)
			       (or (eq definition-id 'environment)
				   (eq definition-id 'inline-math)
				   (eq definition-id 'display-math)))
			    definition-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)

    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match
    (if (and overlay (setq o-match (overlay-get overlay 'start)))
	(progn
	  (push-mark)
	  (goto-char (overlay-get o-match 'delim-end)))
      (message "Not within a LaTeX environment"))))



(defun predictive-latex-jump-to-end-delimiter ()
  "Jump to end of LaTeX environment or equation at point."
  (interactive)

  ;; get innermost LaTeX environment overlay
  (let ((overlay
	 (car (sort (auto-overlays-at-point
		     nil `((identity auto-overlay)
			   (eq set-id predictive)
			   (,(lambda (definition-id)
			       (or (eq definition-id 'environment)
				   (eq definition-id 'inline-math)
				   (eq definition-id 'display-math)))
			    definition-id)))
		    (lambda (a b)
		      (and (<= (- (overlay-end a) (overlay-start a))
			       (- (overlay-end b) (overlay-start b)))))
		    )))
	o-match)

    ;; if we've found an overlay, and it's start-matched, move to its start
    ;; match, otherwise display message
    (if (and overlay (setq o-match (overlay-get overlay 'end)))
	(progn
	  (push-mark)
	  (goto-char (overlay-get o-match 'delim-start)))
      (message "Not within a LaTeX environment"))))




;;;=======================================================================
;;;    Automatic main dictionary switching based on document class

(put 'predictive-latex-docclass 'auto-overlay-parse-function
     'predictive-latex-parse-docclass-match)
(put 'predictive-latex-docclass 'auto-overlay-suicide-function
     'predictive-latex-docclass-suicide)


(defun predictive-latex-parse-docclass-match (o-match)
  ;; Create a new word overlay for a docclass command, and load and set the
  ;; appropriate dictionaries

  ;; create new word overlay and extract docclass name
  (let ((o-new (auto-o-parse-word-match o-match))
	(docclass (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))
	dict)
    ;; save the docclass in an overlay property
    (overlay-put o-match 'docclass-name docclass)
    ;; update the main dict
    (predictive-latex-docclass-change-main-dict docclass)
    ;; return the new overlay
    o-new))



(defun predictive-latex-docclass-suicide (o-match)
  ;; Delete the word overlay for a docclass command, and unload the
  ;; appropriate dictionaries

  (let ((docclass (overlay-get o-match 'docclass-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload the dictionary and restore the default main dictionary
    (predictive-latex-docclass-restore-main-dict docclass)))



(defun predictive-latex-schedule-docclass-update
  (o-self modified &rest unused)
  ;; All docclass overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-docclass-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-docclass-update o-self))))



(defun predictive-latex-docclass-update (o-self)
  ;; Update the main dictionary dictionary after a modification, in case
  ;; docclass has changed

  (let* ((o-match (overlay-get o-self 'start))
	 (docclass (buffer-substring-no-properties
		    (overlay-get o-match 'delim-start)
		    (overlay-get o-match 'delim-end))))
    ;; if we haven't been deleted by a suicide function, and docclass has
    ;; changed, restore main dict and then change to new one
    (when (and (overlay-buffer o-self)
	       (not (string= docclass (overlay-get o-match 'docclass))))
      (predictive-latex-docclass-restore-main-dict
       (overlay-get o-match 'docclass))
      (predictive-latex-docclass-change-main-dict docclass))))



(defun predictive-latex-docclass-change-main-dict (docclass)
  ;; If there is a dictionary associated with the docclass matched by OVERLAY,
  ;; load it and change the main dict

  ;; look for a dictionary associated with the docclass
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))

      ;; if loading of any of the dictionaries in the list fails, unload those
      ;; which succeeded and don't change dictionary
      (if (not (catch 'failed
		 (mapc (lambda (dic)
			 (unless (predictive-load-dict dic)
			   (throw 'failed nil)))
		       dict-list)))
	  (progn
	    (mapc 'predictive-unload-dict dict-list)
	    (message "Failed to load \"%s\" docclass dictionary\"
 - main dictionary NOT changed" docclass))

	;; if loading was successful, unload the old main dictionary
	(mapc 'predictive-unload-dict
	      (if predictive-buffer-dict
		  (if (listp predictive-buffer-dict)
		      predictive-buffer-dict
		    (list predictive-buffer-dict))
		(if (listp predictive-main-dict)
		    predictive-main-dict
		  (list predictive-main-dict))))
	;; if not using a buffer-local dict, simply set new main dictionary
	(if (not predictive-use-buffer-local-dict)
	    (setq predictive-buffer-dict dict-list)
	  ;; otherwise, re-load the buffer-local dict (which will create a new
	  ;; meta-dict and set `predictive-buffer-dict' appropriately)
	  (predictive-load-buffer-local-dict dict-list))
	))))



(defun predictive-latex-docclass-restore-main-dict (docclass)
  ;; Unload any dictionary that has been loaded for DOCCLASS, and restore the
  ;; default main dict
  (let ((dict-list (assoc docclass predictive-latex-docclass-alist)))
    (when dict-list
      (setq dict-list (cdr dict-list))
      (when (atom dict-list) (setq dict-list (list dict-list)))
      (mapc 'predictive-unload-dict dict-list)
      ;; if not using a buffer-local dict, simply reset main dictionary
      (if (not predictive-use-buffer-local-dict)
	  (setq predictive-buffer-dict nil)
	;; otherwise, re-load the buffer-local dict (which will create a new
	;; meta-dict and set `predictive-buffer-dict') and add latex
	;; dictionaries to list
	(predictive-load-buffer-local-dict))
      )))




;;;=======================================================================
;;;  Automatic loading and unloading of LaTeX package dictionaries etc.

(put 'predictive-latex-usepackage 'auto-overlay-parse-function
     'predictive-latex-parse-usepackage-match)
(put 'predictive-latex-usepackage 'auto-overlay-suicide-function
     'predictive-latex-usepackage-suicide)


(defun predictive-latex-parse-usepackage-match (o-match)
  ;; Create a new word overlay for a usepackage command, and load the
  ;; appropriate dictionaries

  ;; create new word overlay
  (let ((o-new (auto-o-parse-word-match o-match))
	package)
    ;; extract package name
    (setq package (buffer-substring-no-properties
		   (overlay-get o-match 'delim-start)
		   (overlay-get o-match 'delim-end)))

    ;; save package name in overlay property
    (overlay-put o-match 'package-name package)
    ;; load package dictionaries and run the load function
    (predictive-latex-load-package package)
    ;; add change function to overlay modification hooks
    (overlay-put o-new 'modification-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'modification-hooks)))
    (overlay-put o-new 'insert-in-front-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-in-front-hooks)))
    (overlay-put o-new 'insert-behind-hooks
		 (cons 'predictive-latex-schedule-usepackage-update
		       (overlay-get o-new 'insert-behind-hooks)))
    ;; return the new overlay
    o-new))



(defun predictive-latex-usepackage-suicide (o-match)
  ;; Delete the word overlay for a usepackage command, and unload the
  ;; appropriate dictionaries

  (let ((package (overlay-get o-match 'package-name)))
    ;; delete the overlay
    (auto-o-delete-overlay (overlay-get o-match 'parent))
    ;; unload package dictionaries and run the unload function
    (predictive-latex-unload-package package)))



(defun predictive-latex-schedule-usepackage-update
  (o-self modified &rest unused)
  ;; All usepackage overlay modification hooks are set to this function, which
  ;; schedules `predictive-latex-usepackage-update' to run after any suicide
  ;; functions have been called
  (unless modified
    (add-to-list 'auto-o-pending-post-suicide
		 (list 'predictive-latex-usepackage-update o-self))))



(defun predictive-latex-usepackage-update (o-self)
  ;; Update the package dictionaries and re-run the load function after a
  ;; modification, in case package name has changed

  (let (package)
    ;; if we haven't been deleted by a suicide function...
    (when (overlay-buffer o-self)
      ;; unload old package dictionaries and run unload function
      (predictive-latex-unload-package
       (overlay-get (overlay-get o-self 'start) 'package-name))
      ;; extract package name
      (setq package (buffer-substring-no-properties
		     (overlay-start o-self)
		     (overlay-end o-self)))
      ;; load new package dictionaries and run load function
      (overlay-put (overlay-get o-self 'start) 'package-name package)
      (predictive-latex-load-package package))))



(defun predictive-latex-load-package (package)
  "Load a LaTeX PACKAGE into the current buffer.
This loads the package dictionary and runs the load functions for
the package, if they exist."
  (interactive "sPackage name: ")

  (let (dict func loaded)
    ;; try to load package dictionaries and add them to the appropriate lists
    ;; if they exists
    (dolist (dic predictive-latex-dict-classes)
      (setq dict (concat (cdr dic) package))
      (when (predictive-load-dict dict)
	(setq loaded t)
	(setq dict (intern-soft dict))
	(if (and (listp (symbol-value (car dic)))
		 (not (dictree-p (symbol-value (car dic)))))
	    (nconc (symbol-value (car dic)) (list dict))
	  (set (car dic) (list (symbol-value (car dic)) dict)))))

    ;; try to load lisp library for the package
    (require (intern (concat "predictive-latex-" package)) nil t)
    ;; run load function for package, if one is defined
    (setq func (cdr (assoc package predictive-latex-usepackage-functions)))
    (when func
      (funcall func 1)
      (setq loaded t))
    ;; display message if we've loaded something
    (when loaded
      (message (format "LaTeX package \"%s\" loaded" package)))))



(defun predictive-latex-unload-package (package)
  "Unload a LaTeX PACKAGE from the current buffer.
This unloads the dictionary and runs the unload functions, if
they exist."
  (interactive "sPackage name: ")
  ;; FIXME: ought to complete on loaded package names when called
  ;;        interactively

  (let (dict)
    ;; unload any package dictionaries
    (dolist (dic predictive-latex-dict-classes)
      (when (setq dict (intern-soft (concat (cdr dic) package)))
	(predictive-unload-dict (symbol-value dict))
	(when (and (listp (symbol-value (car dic)))
		   (not (dictree-p (symbol-value (car dic)))))
	  ;; we don't use "(set ... (delq ..." here because other variables
	  ;; may share structure with the dictionary list variables, and the
	  ;; element we want to delete can not be the first one, as that is
	  ;; always the standard dictionary
	  (delq dict (symbol-value (car dic)))))))

  ;; try to load lisp library for the package
  (require (intern (concat "predictive-latex-" package)) nil t)
  ;; run unload function for package, if one is defined
  (let ((func (cdr (assoc package predictive-latex-usepackage-functions))))
    (when func (funcall func -1)))
  ;; display informative message
  (message (format "LaTeX package \"%s\" unloaded" package)))




;;;=============================================================
;;;                Completion-browser functions

(defun predictive-latex-construct-browser-menu (overlay)
  "Construct the LaTeX browser menu keymap."

  ;; construct menu, dropping the last two entries which are a separator and a
  ;; link back to the basic completion menu (would just redisplay this menu,
  ;; since we're using the browser as the default menu for LaTeX commands)
  (let ((menu (completion-construct-browser-menu
	       overlay 'predictive-latex-browser-menu-item)))
    (setq menu (butlast menu 2))))



(defun predictive-latex-browser-menu-item (cmpl ignored1 ignored2 overlay)
  "Construct predictive LaTeX completion browser menu item."

  (let (submenu)
    ;; search for a match
    (catch 'match
      (dolist (def predictive-latex-browser-submenu-alist)
	(when (and (string-match (car def) cmpl)
		   (= (match-beginning 0) 0)
		   (= (match-end 0) (length cmpl)))
	  (setq submenu (cdr def))
	  (throw 'match t))))

    (cond
     ;;  if entry has a submenu definition, construct sub-menu for it
     (submenu
      ;; if submenu definition is a function, call it
      (when (functionp submenu)
	(setq submenu (funcall submenu cmpl)))
      ;; if submenu definition is a symbol, evaluate it
      (when (symbolp submenu) (setq submenu (symbol-value submenu)))
      ;; if submenu definition is a dictionary or list of dictionaries,
      ;; construct submenu entries from dictionary words
      (cond
       ;; dictionary, get all words in dict
       ((dictree-p submenu)
	(setq submenu
	      (dictree-mapcar (lambda (word data)
				(concat cmpl "{" word "}"))
			      submenu)))
       ;; if submenu definition is a list of dictionaries or symbols, expand
       ;; all symbols in list to get list of dictionaries, then complete empty
       ;; string to get all words in dicts
       ((and (listp submenu)
	     (or (dictree-p (car submenu)) (symbolp (car submenu))))
	(dictree-complete
	 (mapcar (lambda (dic) (if (dictree-p dic) dic (symbol-value dic)))
		 submenu)
	 "" nil nil nil nil nil
	 (lambda (word data) (concat cmpl "{" word "}")))))

      ;; create sub-menu keymap
      (setq submenu (completion-browser-sub-menu
		     submenu
		     'predictive-latex-browser-menu-item
		     'completion-browser-sub-menu
		     overlay))
      ;; add completion itself to the menu
      (define-key submenu [separator-item-sub-menu] '(menu-item "--"))
      (define-key submenu [completion-insert-root]
	(list 'menu-item cmpl
	      `(lambda ()
		 (list ,(if (stringp cmpl) cmpl (car cmpl))
		       ,(if (stringp cmpl)
			    (length (overlay-get overlay 'prefix))
			  (cdr cmpl))))))
      ;; return the menu keymap
      submenu)


     ;; if entry does not match any submenu definition, create a selectable
     ;; completion item
     (t `(lambda ()
	   (list ,(if (stringp cmpl) cmpl (car cmpl))
		 ,(if (stringp cmpl)
		      (length (overlay-get overlay 'prefix))
		    (cdr cmpl))))))))




;;;=============================================================
;;;               Miscelaneous utility functions

(defun predictive-latex-completion-add-till-regexp (regexp)
  "Add characters up to REGEXP from a completion candidate,
then cause `completion-self-insert' to add the last typed
character and re-complete.

Intended to be used as the \"resolve\" entry in
`completion-dynamic-syntax-alist' or
`completion-dynamic-override-syntax-alist'."

  (let (overlay completion)
    ;; if completion characters contain REGEXP, accept characters up to first
    ;; regexp match, and add them to the completion overlay prefix
    (when (and (setq overlay (completion-ui-overlay-at-point))
	       (setq completion (buffer-substring-no-properties
				 (overlay-start overlay)
				 (overlay-end overlay)))
	       (string-match regexp completion))
      (move-overlay overlay
		    (+ (overlay-start overlay) (match-beginning 0))
		    (overlay-end overlay))
      (overlay-put overlay 'prefix
		   (concat (overlay-get overlay 'prefix)
			   (substring completion 0 (match-beginning 0))))
      (overlay-put overlay 'prefix-length
		   (+ (overlay-get overlay 'prefix-length)
		      (match-beginning 0)))
      (goto-char (overlay-start overlay))))

  ;; return 'add, causing `completion-self-insert' to add last typed character
  ;; to the prefix
  'add)


(defun predictive-latex-odd-backslash-p ()
  ;; return non-nil if there are an odd number of \'s before point, otherwise
  ;; return nil
  (let ((i 0))
    (save-excursion (while (eq (char-before) ?\\) (backward-char) (incf i)))
    (= (mod i 2) 1)))


(defun predictive-latex-single-char-command-resolve-behaviour ()
  ;; return appropriate `auto-completion-syntax-override-alist' resolve
  ;; behaviour for a character that can form a single-character LaTeX command
  (if (predictive-latex-odd-backslash-p)
      'add predictive-latex-punctuation-resolve-behaviour))


(defun predictive-latex-single-char-command-completion-behaviour ()
  ;; return appropriate `auto-completion-syntax-override-alist' completion
  ;; behaviour for a character that can form a single-character LaTeX command
  (if (save-excursion (forward-char -1) (predictive-latex-odd-backslash-p))
      predictive-latex-word-completion-behaviour 'none))


(defun predictive-latex-smart-open-brace-completion-behaviour ()
  ;; do something smart when inserting a '{' character, and return appropriate
  ;; `auto-completion-syntax-override-source' behaviour
  (let ((source (auto-completion-source)))
    (cond
     ((completion-ui-source-derives-from-p
       source '(predictive-latex-env
		predictive-latex-label
		predictive-latex-docclass
		predictive-latex-bibstyle))
      (complete-in-buffer-1 source "" 'not-set 'auto)
      'none)
     (t (predictive-latex-single-char-command-completion-behaviour)))))


(defun predictive-latex-smart-quote-insert-behaviour ()
  ;; do smart quote insertion, and return appropriate
  ;;`auto-completion-syntax-override-alist' insertion behaviour
  (cond
   ((predictive-latex-odd-backslash-p) t)
   ((eq (char-before) ?\\)
    (insert TeX-open-quote)
    nil)
   ((fboundp 'TeX-insert-quote)
    (TeX-insert-quote nil)
    nil)
   (t t)))


(defun predictive-latex-smart-within-braces-resolve-behaviour
  (&optional terminator)
  ;; overwrite entire THING if inserting new character at beginning of word
  ;; within braces, and word being overritten is immediately followed by text
  ;; matching TERMINATOR regexp.
  (when completion-overwrite
    (unless terminator (setq terminator "}"))
    (let ((source (auto-completion-source))
	  (pos (point)) word-thing bounds)
      (when (and source
		 (setq bounds
		       (bounds-of-thing-at-point
			(completion-ui-source-word-thing source)))
		 (= (car bounds) pos)
		 (save-excursion
		   (goto-char (cdr bounds))
		   (looking-at terminator)))
	(delete-region pos (cdr bounds))))
    'add))


(defun predictive-latex-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(dotimes (i (- n))
	  ;; make sure we're at the end of a word
	  (when (re-search-backward "\\\\\\|\\w\\|\\*" nil t)
	    (forward-char))
	  ;; if point is within or just after a sequence of \'s, go
	  ;; backwards for the correct number of \'s
	  (if (eq (char-before) ?\\)
	      (let ((pos (point)))
		(save-excursion
		  (while (eq (char-before) ?\\) (backward-char))
		  (setq pos (- pos (point))))
		(if (= (mod pos 2) 1) (backward-char) (backward-char 2)))
	    ;; otherwise, go back one word, plus one \ if there's an odd
	    ;; number of them before it
	    (backward-word 1)  ; argument not optional in Emacs 21
	    (when (and (not (bobp)) (eq (char-before) ?\\))
	      (let ((pos (point)))
		(save-excursion
		  (while (eq (char-before) ?\\) (backward-char))
		  (setq pos (- pos (point))))
		(when (= (mod pos 2) 1) (backward-char))))
	    )))

    ;; going forwards...
    (unless (eobp)
      ;; deal with point within sequence of \'s
      (when (eq (char-after) ?\\)
	(let ((pos (point)))
	  (save-excursion
	    (while (eq (char-before) ?\\) (backward-char))
	    (setq pos (- pos (point))))
	  (when (= (mod pos 2) 1) (backward-char))))
      ;; go forward, counting \ as part of word, \\ as entire word
      (dotimes (i (or n 1))
	(if (or (and (eq (char-before) ?\\)
		     (not (= (char-syntax (char-before)) ?w)))
		(and (char-before)
		     (= (char-syntax (char-before)) ?w)
		     (eq (char-after) ?*)))
	    (forward-char)
	  (when (re-search-forward "\\\\\\|\\w" nil t)
	    (backward-char))
	  (re-search-forward "\\\\\\W\\|\\\\\\w+\\|\\w+" nil t)
	  (cond
	   ((eq (char-before) ?\n) (backward-char))
	   ((eq (char-after) ?*) (forward-char))))))))



(defun predictive-latex-label-forward-word (&optional n)
  ;; going backwards...
  (if (and n (< n 0))
      (unless (bobp)
	(setq n (- n))
	(when (eq (char-before) ?\\)
	  (while (eq (char-before) ?\\) (backward-char))
	  (setq n (1- n)))
	(dotimes (i n)
	  (when (and (char-before) (= (char-syntax (char-before)) ?w))
	    (backward-word 1))  ; argument not optional in Emacs 21
	  (while (and (char-before)
		      (or (= (char-syntax (char-before)) ?w)
			  (= (char-syntax (char-before)) ?_)
			  (and (= (char-syntax (char-before)) ?.)
			       (/= (char-before) ?{))))
	    (backward-char))))
    ;; going forwards...
    (unless (eobp)
      (setq n (if n n 1))
      (dotimes (i n)
	(when (and (char-after) (= (char-syntax (char-after)) ?w))
	  (forward-word 1))  ; argument not optional in Emacs 21
	(while (and (char-after)
		    (or (= (char-syntax (char-after)) ?w)
			(= (char-syntax (char-after)) ?_)
			(and (= (char-syntax (char-after)) ?.)
			     (/= (char-after) ?}))))
	  (forward-char))))))



(provide 'predictive-latex)

;;; predictive-latex.el ends here
