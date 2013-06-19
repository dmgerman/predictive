

(defvar latex-eldoc-obarray (make-vector 41 0))


;;;###autoload
(define-minor-mode latex-eldoc-mode
  "Toggle echo area display of LaTeX commands at point (ElDoc mode).
With a prefix argument ARG, enable LaTeX-ElDoc mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
LaTeX-ElDoc mode if ARG is omitted or nil.

LaTeX-ElDoc mode is a buffer-local minor mode.  When enabled, the
echo area displays information about a LaTeX command or
environment in the text where point is."
  :group 'eldoc
  :lighter eldoc-minor-mode-string
  (if latex-eldoc-mode
      (progn
	(set (make-local-variable 'eldoc-documentation-function)
	     'latex-eldoc-function)
	(eldoc-mode 1))
    (kill-local-variable 'eldoc-documentation-function)
    (eldoc-mode -1)))


(defun latex-eldoc-function ()
  "Return doc string for the current context, or nil."
  (let ((words-include-escapes t))
    (symbol-value (intern-soft (thing-at-point 'word) latex-eldoc-obarray))))



;;; LaTeX

(set (intern "tabular" latex-eldoc-obarray)
     #("\\begin{tabular}[<pos>]{<cols>}...\\end{tabular}  Tabular material"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       16 21 (face (font-lock-variable-name-face))
       23 29 (face (font-lock-variable-name-face))
       33 37 (face (font-lock-keyword-face font-latex-sedate-face))
       38 45 (face (font-lock-function-name-face))))

(set (intern "tabular*" latex-eldoc-obarray)
     #("\\begin{tabular*}{<width>}[<pos>]{<cols>}...\\end{tabular*}  Fixed width tabular material"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 15 (face (font-lock-function-name-face))
       17 24 (face (font-lock-variable-name-face))
       26 31 (face (font-lock-variable-name-face))
       33 39 (face (font-lock-variable-name-face))
       43 47 (face (font-lock-keyword-face font-latex-sedate-face))
       48 56 (face (font-lock-function-name-face))))

(set (intern "array" latex-eldoc-obarray)
     #("\\begin{array}[<pos>]{<cols>}...\\end{array}  Math mode tabular material"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 12 (face (font-lock-function-name-face))
       14 19 (face (font-lock-variable-name-face))
       21 27 (face (font-lock-variable-name-face))
       31 35 (face (font-lock-keyword-face font-latex-sedate-face))
       36 41 (face (font-lock-function-name-face))))

(set (intern "\\multicolumn" latex-eldoc-obarray)
     #("\\multicolumn{<#cols>}{<col>}{<content>}  Table content spanning multiple columns"
       0 12 (face (font-lock-keyword-face font-latex-sedate-face))
       13 20 (face (font-lock-variable-name-face))
       22 27 (face (font-lock-variable-name-face))
       29 38 (face (font-lock-variable-name-face))))

(set (intern "\\multirow" latex-eldoc-obarray)
     #("\\multirow{<#rows>}{<width>|*}{<content>}  Table content spanning multiple rows"
       0 9 (face (font-lock-keyword-face font-latex-sedate-face))
       10 17 (face (font-lock-variable-name-face))
       19 28 (face (font-lock-variable-name-face))
       30 39 (face (font-lock-variable-name-face))))



;;; amsmath

(set (intern "align" latex-eldoc-obarray)
     #("\\begin{align}...\\end{align}  Aligned equation group"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 12 (face (font-lock-function-name-face))
       17 20 (face (font-lock-keyword-face font-latex-sedate-face))
       21 26 (face (font-lock-function-name-face))))

(set (intern "align*" latex-eldoc-obarray)
     #("\\begin{align*}...\\end{align*}  Unnumbered aligned equation group"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 21 (face (font-lock-keyword-face font-latex-sedate-face))
       22 28 (face (font-lock-function-name-face))))

(set (intern "alignat" latex-eldoc-obarray)
     #("\\begin{alignat}{<#cols>}...\\end{alignat}  Aligned equation group with explicit spacing between columns"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       16 23 (face (font-lock-variable-name-face))
       27 31 (face (font-lock-keyword-face font-latex-sedate-face))
       32 39 (face (font-lock-function-name-face))))

(set (intern "alignat*" latex-eldoc-obarray)
     #("\\begin{alignat*}{<#cols>}...\\end{alignat*}  Unnumbered aligned equation group with explicit column spacing"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 15 (face (font-lock-function-name-face))
       17 24 (face (font-lock-variable-name-face))
       28 31 (face (font-lock-keyword-face font-latex-sedate-face))
       32 40 (face (font-lock-function-name-face))))

(set (intern "aligned" latex-eldoc-obarray)
     #("\\begin{aligned}...\\end{aligned}  Aligned equation building block"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "alignedat" latex-eldoc-obarray)
      #("\\begin{alignedat}{<#cols>}...\\end{alignedat}  Aligned equation building block with explicit column spacing"
	0 6 (face (font-lock-keyword-face font-latex-sedate-face))
	7 16 (face (font-lock-function-name-face))
	18 25 (face (font-lock-variable-name-face))
	29 33 (face (font-lock-keyword-face font-latex-sedate-face))
	34 43 (face (font-lock-function-name-face))))

(set (intern "matrix" latex-eldoc-obarray)
     #("\\begin{matrix}...\\end{matrix}  Matrix without delimiters"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 13 (face (font-lock-function-name-face))
       17 21 (face (font-lock-keyword-face font-latex-sedate-face))
       22 28 (face (font-lock-function-name-face))))

(set (intern "pmatrix" latex-eldoc-obarray)
     #("\\begin{pmatrix}...\\end{pmatrix}  Matrix delimited by ( )"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "bmatrix" latex-eldoc-obarray)
     #("\\begin{bmatrix}...\\end{bmatrix}  Matrix delimited by [ ]"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "Bmatrix" latex-eldoc-obarray)
     #("\\begin{Bmatrix}...\\end{Bmatrix}  Matrix delimited by { }"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "Vmatrix" latex-eldoc-obarray)
     #("\\begin{Vmatrix}...\\end{Vmatrix}  Matrix delimited by || ||"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "vmatrix" latex-eldoc-obarray)
     #("\\begin{vmatrix}...\\end{vmatrix}  Matrix delimited by | |"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 14 (face (font-lock-function-name-face))
       18 22 (face (font-lock-keyword-face font-latex-sedate-face))
       23 30 (face (font-lock-function-name-face))))

(set (intern "smallmatrix" latex-eldoc-obarray)
     #("\\begin{smallmatrix}...\\end{smallmatrix}  Matrix suitable for inline math"
       0 6 (face (font-lock-keyword-face font-latex-sedate-face))
       7 18 (face (font-lock-function-name-face))
       22 26 (face (font-lock-keyword-face font-latex-sedate-face))
       27 38 (face (font-lock-function-name-face))))


;;; mathtools

(set (intern "\\ArrowBetweenLines" latex-eldoc-obarray)
     #("\\ArrowBetweenLines[<symbol>]"
       0 18 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\ArrowBetweenLines*" latex-eldoc-obarray)
     #("\\ArrowBetweenLines*[<symbol>]"
       0 19 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\MoveEqLeft" latex-eldoc-obarray)
     #("\\MoveEqLeft[<dimen>]"
       0 11 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\adjustlimits" latex-eldoc-obarray)
      #("\\adjustlimits{<operator1>}_{<limit1>} {<operator2>}_{<limit2>}"
	0 13 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\cramped" latex-eldoc-obarray)
     #("\\cramped[<mathstyle>]{<math>}"
       0 8 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\mathclap" latex-eldoc-obarray)
     #("\\mathclap[<mathstyle>]{<math>}"
       0 9 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\mathllap" latex-eldoc-obarray)
     #("\\mathllap[<mathstyle>]{<math>}"
       0 9 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\mathmakebox" latex-eldoc-obarray)
     #("\\mathmakebox[<width>][<pos>]{<math>}"
       0 12 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\mathrlap" latex-eldoc-obarray)
     #("\\mathrlap[<mathstyle>]{<math>}"
       0 9 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\overbracket" latex-eldoc-obarray)
     #("\\overbracket[<rule thickness>][<bracket height>]{<arg>}"
       0 12 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\prescript" latex-eldoc-obarray)
     #("\\prescript{<sup>}{<sub>}{<arg>}"
       0 10 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\shoveleft" latex-eldoc-obarray)
     #("\\shoveleft[<dimen>]{<arg>}"
       0 10 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\shoveright" latex-eldoc-obarray)
     #("\\shoveright{<dimen>}{<arg>}"
       0 11 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\smashoperator" latex-eldoc-obarray)
     #("\\smashoperator[<pos>]{<operator with limits>}"
       0 14 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\splitdfrac" latex-eldoc-obarray)
     #("\\splitfrac{<numer>}{<demon>}"
       0 11 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\splitfrac" latex-eldoc-obarray)
     #("\\splitfrac{<numer>}{<demon>}"
       0 10 (face (font-lock-keyword-face font-latex-sedate-face))))

(set (intern "\\underbracket" latex-eldoc-obarray)
     #("\\underbracket[<rule thickness>][<bracket height>]{<arg>}"
       0 13 (face (font-lock-keyword-face font-latex-sedate-face))))


(provide 'latex-eldoc)
