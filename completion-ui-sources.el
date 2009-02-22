
;;; completion-ui-sources.el --- Completion-UI completion sources


;; Copyright (C) 2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: completion, UI, user interface, sources
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
;; This package provides user-interfaces for a number of Emacs in-buffer
;; completion mechanisms, currently: dabbrevs, etags, elisp, predictive-mode,
;; and Semantic are supported. (Note that, for predictive-mode and Semantic
;; support, you will need to install the corresponding packages separately.)
;; It is partly intended as a set of examples for the completion-UI package,
;; but is also useful in and of itself.
;;
;; See also `predictive-mode' from the Predictive Completion package,
;; available separately from the above URL, which provides an advanced
;; user-interface for predictive completion.
;;
;;
;; INSTALLING
;; ----------
;; To install this package, save this file and the accompanying
;; "completion-ui.el" library to a directory in your `load-path' (you can view
;; the current `load-path' using "C-h v load-path" within Emacs), then add the
;; following line to your .emacs startup file:
;;
;;    (require 'completion-ui-examples)
;;
;;
;; ENABLING
;; --------
;; To enable one of the completion user-interfaces in the current buffer, use
;;
;;   M-x completion-ui-enable-<type>
;;
;; replacing <type> with the type of completion you want to use, currently one
;; of: "dabbrev", "etags", "elisp", "predictive", or "semantic". To disable
;; the completion user-interface, use
;;
;;   M-x completion-ui-disable
;;
;; To customize the behaviour of the completion user-interface, see the
;; `completion-ui' customization group.
;;
;;
;; USING
;; -----
;; When a completion user-interface is enabled, the following default key
;; bindings can be used to complete the word at or next to the point:
;;
;; M-<tab>  M-/  M-S-<tab>  M-?
;;   Complete word at point.
;;
;;
;; If you want to automatically complete words as you type them, enable
;; `auto-completion-mode':
;;
;;   M-x auto-completion-mode
;;
;; Note that `auto-completion-mode' is not very useful if the completion
;; mechanism takes a long time to find completions, so Semantic support in
;; particular won't work all that well.
;;
;;
;; When completing a word, the following default key bindings are available:
;;
;; M-<tab>  M-/
;;   Cycle through completions.
;;
;; M-S-<tab>  M-?
;;   Cycle backwards through completions.
;;
;; C-<ret>
;;   Accept the current completion.
;;
;; C-<del>
;;    Reject the current completion.
;;
;; <tab>
;;    Traditional tab-completion, i.e. insert longest common substring.
;;
;; C-<tab>
;;    Accept current completion and re-complete the resulting word.
;;
;; S-<down>
;;    Display the completion tooltip (then use <up> and <down> to cycle).
;;
;; M-<down>
;;    Display the completion menu.
;;
;; C-<down>
;;    Display the completion pop-up frame.
;;
;;
;; CUSTOMIZING
;; -----------
;; The completion user-interface can be heavily customized and tweaked to suit
;; your every desire, via the `completion-ui' customization group:
;;
;;   M-x customize-group <ret> completion-ui <ret>
;;
;; All the customization options and settings are documented there.


(provide 'completion-ui-sources)
(require 'completion-ui)


;; get rid of compiler warnings
(eval-when-compile
  (defvar semanticdb-find-default-throttle nil)
  (defun dabbrev--reset-global-variables () nil)
  (defun dabbrev--find-all-expansions (arg1 arg2) nil)
  (defun tags-lazy-completion-table () nil)
  (defun semantic-idle-summary-useful-context-p () nil)
  (defun semantic-ctxt-current-symbol (arg1) nil)
  (defun semantic-analyze-current-context () nil)
  (defun semantic-analyze-possible-completions (arg1) nil))


;;;=========================================================
;;;                     dabbrevs

(completion-ui-register-source
 (lambda (prefix)
   (require 'dabbrev)
   (dabbrev--reset-global-variables)
   (dabbrev--find-all-expansions prefix case-fold-search))
 :name 'dabbrev)


;;;=========================================================
;;;                        etags

(completion-ui-register-source
 (lambda (prefix)
   (require 'etags)
   (all-completions prefix (tags-lazy-completion-table)))
 :name 'etags)


;;;=========================================================
;;;                        Elisp

(completion-ui-register-source
 'all-completions
 :completion-args 1
 :other-args '(obarray)
 :name 'Elisp)


;;;=========================================================
;;;                        Elisp

(defun completion--filename-wrapper (prefix)
  ;; Return filename completions of prefix
  (let ((dir (file-name-directory prefix))
	completions)
    (mapc (lambda (file)
	    (unless (or (string= file "../") (string= file "./"))
	      (push (concat dir file) completions)))
	  (file-name-all-completions
	   (file-name-nondirectory prefix) dir))
    (nreverse completions)))


(completion-ui-register-source
 'completion--filename-wrapper
 :name 'files)


;;;=========================================================
;;;                        NXML

(completion-ui-register-source
 'rng-complete-qname-function
 :completion-args 1
 :other-args '(t t)
 :name 'nxml)


;;;=========================================================
;;;                        Semantic

(defun completion--semantic-prefix-wrapper ()
  ;; Return prefix at point that Semantic would complete.
  (require 'semantic-ia)
  (when (semantic-idle-summary-useful-context-p)
    (let ((prefix (semantic-ctxt-current-symbol (point))))
      (setq prefix (nth (1- (length prefix)) prefix))
      (set-text-properties 0 (length prefix) nil prefix)
      prefix)))


(defun completion--semantic-wrapper (prefix &optional maxnum)
  ;; Return list of Semantic completions for PREFIX at point. Optional
  ;; argument MAXNUM is the maximum number of completions to return.
  (require 'semantic-ia)
  (when (semantic-idle-summary-useful-context-p)
    (let* (
	   ;; don't go loading in oodles of header libraries for minor
	   ;; completions if using auto-completion-mode
	   ;; FIXME: don't do this iff the user invoked completion manually
	   (semanticdb-find-default-throttle
	    (when (and (featurep 'semanticdb-find)
		       auto-completion-mode)
	      (remq 'unloaded semanticdb-find-default-throttle)))

	   (ctxt (semantic-analyze-current-context))
	   (acomp (semantic-analyze-possible-completions ctxt)))
      (when (and maxnum (> (length acomp) maxnum))
	(setq acomp (butlast acomp (- (length acomp) maxnum))))
      (mapcar 'semantic-tag-name acomp))))


(defun completion--semantic-enable-auto-completion nil
  ;; set variables buffer-locally when enabling Semantic auto-completion
  (when (eq auto-completion-source 'Semantic)
    (set (make-local-variable 'auto-completion-override-syntax-alist)
	 '((?. . (add word))))))


(defun completion--semantic-disable-auto-completion nil
  ;; unset buffer-local variables when disabling Semantic auto-completion
  (when (eq auto-completion-source 'Semantic)
    (kill-local-variable 'auto-completion-override-syntax-alist)))


(add-hook 'auto-completion-mode-enable-hook
	  'completion--semantic-enable-auto-completion)
(add-hook 'auto-completion-mode-disable-hook
	  'completion--semantic-disable-auto-completion)


;; register the Semantic source
(completion-ui-register-source
 'completion--semantic-wrapper
 :prefix-function completion--semantic-prefix-wrapper
 :name 'Semantic)



;;; completion-ui-examples.el end here
