;!=== Lathe.lisp Machine-Learning Library ===!
;|      Lathe Software Foundation 2020       |
;|          MIT Permissive License           |

; Make Packages
(make-package :stats)
(make-package :preprocess)
(make-package :models)
; ================ Stats ================__
(in-package stats)
; Summation (Added to avoid reducing so frequently)
(defun sum  (x) (reduce '+ y)
)
; Mean
(defun mean (x) (/ (sum x) (length x))
)
; Variance
(defun variance (x) (* (/ (sum x) (mean x)) (/ (sum x) (mean x)))
)
; Standard Deviation
(defun std (x) (setq μ (mean x)) (loop for y in x do
  (* (- x μ) (- x μ))) (sqrt (mean x))
)
; Standard Error
(defun ste (x) (/ (std x) (length x))
)
