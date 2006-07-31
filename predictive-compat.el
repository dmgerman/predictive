
;;; predictive-compat.el --- compatability functions for predictive package

;; Copyright (C) 2006 Toby Cubitt

;; Author: Toby Cubitt
;; Version: 0.1
;; Keywords: predictive, compatability
;; URL: http://www.dr-qubit.org/emacs.php

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
;; Version 0.1
;; * initial release


;;; Code:

(provide 'predictive-compat)



(defun predictive-compat-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults to the
position of point in the selected window."
  
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  ;; get window-relative position in units of characters
  (let* ((x-y (compute-motion (window-start) '(0 . 0)
			      position
			      (cons (window-width) (window-height))
			      (window-width)
			      (cons (window-hscroll) 0) ; prob. shouldn't be 0
			      window))
	 (x (nth 1 x-y))
	 (y (nth 2 x-y))
	 (offset (predictive-compat-window-offsets window))
	 (restore (mouse-pixel-position))
	 pixel-pos)
    
    ;; move and restore mouse position using position in units of characters
    ;; to get position in pixels
    (set-mouse-position (window-frame window)
			(+ x (car offset)) (+ y (cdr offset)))
    (setq pixel-pos (cdr (mouse-pixel-position)))
    (set-mouse-pixel-position (car restore) (cadr restore) (cddr restore))
    
    ;; return pixel position
    (setcdr pixel-pos (- (cdr pixel-pos)
			 (/ (frame-char-height (window-frame window)) 2)))
    pixel-pos)
)
    


(defun predictive-compat-posn-at-point-as-event
  (&optional position window dx dy)
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

  ;; get window-relative position in units of characters
  (let* ((x-y (compute-motion (window-start) '(0 . 0)
			      position
			      (cons (window-width) (window-height))
			      (window-width)
			      (cons (window-hscroll) 0) ; shouldn't be 0
			      window))
	 (x (nth 1 x-y))
	 (y (nth 2 x-y))
	 (offset (predictive-compat-window-offsets window))
	 (restore (mouse-pixel-position))
	 (frame (window-frame window))
	 (edges (window-edges window))
	 pixel-pos)

    ;; move and restore mouse position using position in units of characters
    ;; to get position in pixels
    (set-mouse-position (window-frame window)
			(+ x (car offset)) (+ y (cdr offset)))
    (setq pixel-pos (cdr (mouse-pixel-position)))
    (set-mouse-pixel-position (car restore) (cadr restore) (cddr restore))

    ;; convert pixel position from frame-relative to window-relative (this
    ;; is crude and will fail e.g. if using different sized fonts)
    (setcar pixel-pos (- (car pixel-pos) 1
			 (* (frame-char-width frame) (car edges))))
    (setcdr pixel-pos (- (cdr pixel-pos) 1
			 (* (frame-char-height frame) (nth 1 edges))
			 (/ (frame-char-height frame) 2)))
    
    ;; return a fake event containing the position
    (setcar pixel-pos (+ (car pixel-pos) dx))
    (setcdr pixel-pos (+ (cdr pixel-pos) dy))
    (list 'mouse-1 (list window position pixel-pos)))
)




;;; Borrowed from senator.el:

(defun predictive-compat-window-offsets (&optional window)
  "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET)) in
WINDOW'S frame."
  (let* ((window  (or window (selected-window)))
         (e       (window-edges window))
         (left    (nth 0 e))
         (top     (nth 1 e))
         (right   (nth 2 e))
         (bottom  (nth 3 e))
         (x       (+ left (/ (- right left) 2)))
         (y       (+ top  (/ (- bottom top) 2)))
         (wpos    (coordinates-in-window-p (cons x y) window))
         (xoffset 0)
         (yoffset 0))
    (if (consp wpos)
        (let* ((f  (window-frame window))
               (cy (/ 1.0 (float (frame-char-height f)))))
          (setq xoffset (- x (car wpos))
                yoffset (float (- y (cdr wpos))))
          ;; If Emacs 21 add to:
          ;; - XOFFSET the WINDOW left margin width.
          ;; - YOFFSET the height of header lines above WINDOW.
          (if (> emacs-major-version 20)
              (progn
                (setq wpos    (cons (+ left xoffset) 0.0)
                      bottom  (float bottom))
                (while (< (cdr wpos) bottom)
                  (if (eq (coordinates-in-window-p wpos window)
                          'header-line)
                      (setq yoffset (+ yoffset cy)))
                  (setcdr wpos (+ (cdr wpos) cy)))
                (setq xoffset (floor (+ xoffset
                                        (or (car (window-margins window))
                                            0))))))
          (setq yoffset (floor yoffset))))
    (cons xoffset yoffset))
)



(defun predictive-compat-line-number-at-pos (pos)
  "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
  (1+ (count-lines (point-min) pos))
)

;;; predictive-compat.el ends here
