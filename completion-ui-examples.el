
;;; completion-ui-examples.el --- in-buffer completion user interface for
;;;                               a number of Emacs completion mechanisms


;; Copyright (C) 2008 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
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
;; This package provides user-interfaces for a number of Emacs in-buffer
;; completion mechanisms. Currently: dabbrevs, etags, elisp, and Semantic are
;; supported. (Note that, for Semantic support, you will need to install the
;; Semantic package separately.) It is partly intended as a set of examples
;; for the completion-UI package, but is also useful in and of itself.
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
;;    (require 'completion-ui)
;;
;;
;; ENABLING
;; --------
;; To enable one of the completion user-interfaces in the current buffer, use
;;
;;   M-x completion-ui-enable-<type>
;;
;; replacing <type> with the type of completion you want to use, currently one
;; of: dabbrev, etags, elisp, or semantic. To disable the completion
;; user-interface, use
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


(require 'completion-ui)
(provide 'completion-ui-examples)


(defun completion-ui-disable ()
  "Disable completion user-interface in current buffer."
  (interactive)
  (setq completion-function nil)
  (setq completion-prefix-function 'completion-prefix)
  (kill-local-variable 'auto-completion-syntax-alist)
  (kill-local-variable 'auto-completion-override-syntax-alist)
  (auto-completion-mode -1))



;;;=========================================================
;;;                     dabbrevs

(defun completion--dabbrev-wrapper (prefix maxnum)
  ;; Wrapper around `dabbrev--find-all-completions', to use as a
  ;; `completion-function'.
  (dabbrev--reset-global-variables)
  (let ((completions (dabbrev--find-all-expansions prefix nil)))
    (when maxnum
      (setq completions
	    (butlast completions (- (length completions) maxnum))))
    completions))


(defun completion-ui-enable-dabbrev ()
  "Enable completion user-interface for dabbrevs.

To complete the word at or next to the point, the following key
bindings can be used:
\\<completion-map>
\\[complete-or-cycle-word-at-point] \\[complete-or-cycle-backwards-word-at-point] \t Complete word at point.

When completing a word, the following key bindings are available:

\\[complete-or-cycle-word-at-point] \t\t Cycle through completions.
\\[complete-or-cycle-backwards-word-at-point] \t\t Cycle backwards through completions.
\\<completion-dynamic-map>
\\[completion-accept] \t Accept the current completion.
\\[completion-reject] \t Reject the current completion.
\\[completion-tab-complete] \t\t Insert longest common prefix.
\\[completion-scoot-ahead] \t\t Insert completion and re-complete word.
\\[completion-show-tooltip] \t Display the completion tooltip.\\<completion-tooltip-map>
\\[completion-tooltip-cycle] \t\t Scroll through completions in the tooltip.
\\[completion-tooltip-cycle-backwards] \t\t Scroll backwards through completions in the tooltip.\\<completion-dynamic-map>
\\[completion-show-menu] \t Display the completion menu.
\\[completion-popup-frame] \t Display the completion pop-up frame."
  (interactive)
  (require 'dabbrev)
  (setq completion-function 'completion--dabbrev-wrapper))



;;;=========================================================
;;;                        etags

(defun completion--etags-wrapper (prefix maxnum)
  ;; Wrapper around a call to `all-completions' using `tags-complete-tag', to
  ;; use as a `completion-function'.
  (let ((completions (all-completions prefix 'tags-complete-tag)))
    (when maxnum
      (setq completions
	    (butlast completions (- (length completions) maxnum))))
    completions))


(defun completion-ui-enable-etags ()
  "Enable completion user-interface for etags.

To complete the word at or next to the point, the following key
bindings can be used:
\\<completion-map>
\\[complete-or-cycle-word-at-point] \\[complete-or-cycle-backwards-word-at-point] \t Complete word at point.

When completing a word, the following key bindings are available:

\\[complete-or-cycle-word-at-point] \t\t Cycle through completions.
\\[complete-or-cycle-backwards-word-at-point] \t\t Cycle backwards through completions.
\\<completion-dynamic-map>
\\[completion-accept] \t Accept the current completion.
\\[completion-reject] \t Reject the current completion.
\\[completion-tab-complete] \t\t Insert longest common prefix.
\\[completion-scoot-ahead] \t\t Insert completion and re-complete word.
\\[completion-show-tooltip] \t Display the completion tooltip.\\<completion-tooltip-map>
\\[completion-tooltip-cycle] \t\t Scroll through completions in the tooltip.
\\[completion-tooltip-cycle-backwards] \t\t Scroll backwards through completions in the tooltip.\\<completion-dynamic-map>
\\[completion-show-menu] \t Display the completion menu.
\\[completion-popup-frame] \t Display the completion pop-up frame."
  (interactive)
  (require 'etags)
  (setq completion-function 'completion--etags-wrapper))



;;;=========================================================
;;;                        Elisp

(defun completion--elisp-wrapper (prefix maxnum)
  ;; Wrapper around a call to `all-completions' using `obarray', to use as a
  ;; `completion-function'.
  (let ((completions (all-completions prefix obarray)))
    (when maxnum
      (setq completions
	    (butlast completions (- (length completions) maxnum))))
    completions))


(defun completion-ui-enable-elisp ()
  "Enable completion user-interface for Elisp.

To complete the word at or next to the point, the following key
bindings can be used:
\\<completion-map>
\\[complete-or-cycle-word-at-point] \\[complete-or-cycle-backwards-word-at-point] \t Complete word at point.

When completing a word, the following key bindings are available:

\\[complete-or-cycle-word-at-point] \t\t Cycle through completions.
\\[complete-or-cycle-backwards-word-at-point] \t\t Cycle backwards through completions.
\\<completion-dynamic-map>
\\[completion-accept] \t Accept the current completion.
\\[completion-reject] \t Reject the current completion.
\\[completion-tab-complete] \t\t Insert longest common prefix.
\\[completion-scoot-ahead] \t\t Insert completion and re-complete word.
\\[completion-show-tooltip] \t Display the completion tooltip.\\<completion-tooltip-map>
\\[completion-tooltip-cycle] \t\t Scroll through completions in the tooltip.
\\[completion-tooltip-cycle-backwards] \t\t Scroll backwards through completions in the tooltip.\\<completion-dynamic-map>
\\[completion-show-menu] \t Display the completion menu.
\\[completion-popup-frame] \t Display the completion pop-up frame."

  (interactive)
  (setq completion-function 'completion--elisp-wrapper))



;;;=========================================================
;;;                        Semantic


(defun completion--semantic-prefix-wrapper ()
  ;; Return prefix at point that Semantic would complete.
  (when (semantic-idle-summary-useful-context-p)
    (let ((prefix (semantic-ctxt-current-symbol (point))))
      (setq prefix (nth (1- (length prefix)) prefix))
      (set-text-properties 0 (length prefix) nil prefix)
      prefix)))


(defun completion--semantic-wrapper (prefix maxnum)
  ;; Return list of Semantic completions for PREFIX at point. Optional
  ;; argument MAXNUM is the maximum number of completions to return.
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



(defun completion-ui-enable-semantic ()
  "Enable completion user-interface for Semantic.

To complete the word at or next to the point, the following key
bindings can be used:
\\<completion-map>
\\[complete-or-cycle-word-at-point] \\[complete-or-cycle-backwards-word-at-point] \t Complete word at point.

When completing a word, the following key bindings are available:

\\[complete-or-cycle-word-at-point] \t\t Cycle through completions.
\\[complete-or-cycle-backwards-word-at-point] \t\t Cycle backwards through completions.
\\<completion-dynamic-map>
\\[completion-accept] \t Accept the current completion.
\\[completion-reject] \t Reject the current completion.
\\[completion-tab-complete] \t\t Insert longest common prefix.
\\[completion-scoot-ahead] \t\t Insert completion and re-complete word.
\\[completion-show-tooltip] \t Display the completion tooltip.\\<completion-tooltip-map>
\\[completion-tooltip-cycle] \t\t Scroll through completions in the tooltip.
\\[completion-tooltip-cycle-backwards] \t\t Scroll backwards through completions in the tooltip.\\<completion-dynamic-map>
\\[completion-show-menu] \t Display the completion menu.
\\[completion-popup-frame] \t Display the completion pop-up frame."
  (interactive)
  (setq completion-function 'completion--semantic-wrapper)
  (setq completion-prefix-function 'completion--semantic-prefix-wrapper)
  (make-local-variable 'auto-completion-override-syntax-alist)
  (setq auto-completion-override-syntax-alist '((?. . (add word))))
  (define-key completion-map "." 'completion-self-insert))
