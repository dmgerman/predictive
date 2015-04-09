
;;; predictive-latex-prelude.el --- predictive mode LaTeX prelude package support


;; Copyright (C) 2015 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.1
;; Keywords: predictive, latex, package, prelude
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

(require 'predictive-latex)

;; register package setup function
(predictive-assoc-delete-all "prelude" predictive-latex-usepackage-functions)
(push '("prelude" . predictive-latex-setup-prelude)
      predictive-latex-usepackage-functions)




;;;============================================================
;;;                       Setup function

(defun predictive-latex-setup-prelude (&optional arg)
  ;; With positive ARG, load prelude package support. With negative ARG,
  ;; unload it.
  (cond
   
   ;; --- load prelude support ---
   ((> arg 0)
    (predictive-latex-load-package "microtype")
    (predictive-latex-load-package "amsmath")
    (predictive-latex-load-package "amssymb")
    (predictive-latex-load-package "quantum")
    (predictive-latex-load-package "authblk")
    (predictive-latex-load-package "footmisc")
    (predictive-latex-load-package "paralist")
    (predictive-latex-load-package "enumitem")
    (predictive-latex-load-package "tocloft")
    (predictive-latex-load-package "verbatim")
    (predictive-latex-load-package "color")

    (predictive-latex-load-package "array")
    (predictive-latex-load-package "tabularx")
    (predictive-latex-load-package "longtable")
    (predictive-latex-load-package "multirow")
    (predictive-latex-load-package "rotating")

    (predictive-latex-load-package "biblatex")
    (predictive-latex-load-package "hyperref")
    (predictive-latex-load-package "ntheorem")
    (predictive-latex-load-package "cleveref")
    t)

   ;; --- unload prelude support ---
   ((< arg 0)
    (predictive-latex-unload-package "microtype")
    (predictive-latex-unload-package "amsmath")
    (predictive-latex-unload-package "amssymb")
    (predictive-latex-unload-package "quantum")
    (predictive-latex-unload-package "authblk")
    (predictive-latex-unload-package "footmisc")
    (predictive-latex-unload-package "paralist")
    (predictive-latex-unload-package "enumitem")
    (predictive-latex-unload-package "tocloft")
    (predictive-latex-unload-package "verbatim")
    (predictive-latex-unload-package "color")

    (predictive-latex-unload-package "array")
    (predictive-latex-unload-package "tabularx")
    (predictive-latex-unload-package "longtable")
    (predictive-latex-unload-package "multirow")
    (predictive-latex-unload-package "rotating")

    (predictive-latex-unload-package "biblatex")
    (predictive-latex-unload-package "hyperref")
    (predictive-latex-unload-package "ntheorem")
    (predictive-latex-unload-package "cleveref")
    )))


(provide 'predictive-latex-prelude)

;;; predictive-latex-prelude ends here
