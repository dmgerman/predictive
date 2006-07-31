
;;; auto-overlays-compat.el --- compatability functions for auto-overlays package


;; Copyright (C) 2006 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: auto-overlay, automatic, overlays, compatability
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is part of the Emacs Automatic Overlays package.
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
;; Boston, MA 02111-1307 USA


;;; Change Log:
;;
;; Version 0.1
;; * initial release


;;; Code:

(provide 'auto-overlays-compat)


(defun auto-overlays-compat-line-number-at-pos (pos)
  "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
  (1+ (count-lines (point-min) pos))
)

;;; auto-overlays-compat.el ends here
