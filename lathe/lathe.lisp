;!=== Lathe.lisp Machine-Learning Library ===!
;|      Lathe Software Foundation 2020       |
;|          MIT Permissive License           |

; Make Packages
(make-package :stats)
(make-package :preprocess)
(make-package :models)
(make-package :matrix)
; ================ Stats ================__
(in-package stats)
; Dot (A lot easier)
(defun .* (x y) * (loop for c in x for z in y do (* z c))
)

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
; Correlation Coefficient (r)
(defun correlationcoeff (x y) (setq n (length x))
(setq yn (length y)) (setq Σx (sum x)) (setq Σy (sum y))
)
; Correlation Coefficient of Determination (r^2)
(defun r2 (ŷ y) (setq r (correlationcoeff ŷ y))
(* r r)
)
; Independent T Test
(defun independent_t (sample general) (setq μ (mean general))
    (setq x̄ (mean sample))  (setq n (length sample)) (loop for c in general do
        (* (- c m) (- c m))) (setq m (sqrt general)) (/ (- x̄ μ) (/ m (sqrt n)))
)
; F-Test!
(defun f_test (sample general) (setq σ² (variance general))
 (setq v (variance sample)) (/ v / σ²)
)
; ============= Matrix =============__
(defun gets (x lst)
    ( if (= x 0) (car lst)
         (gets (- x 1) (cdr lst) )
     )
)

(defun getrow (x matrix)
    (gets x matrix)
 )

(defun getcol (x matrix)
    (if (null matrix) nil
     (append (list (gets x (car matrix) )) (getcol x (cdr matrix) ))
        )
    )


(defun helper (line col) (apply #'+ 0 (mapcar #'* line col) ))

(defun helper2 (line matrix x) (if (= -1 x) nil (append
     (list (helper line (getcol (- (length matrix) x)
      matrix))) (helper2 line matrix (- x 1))) ))


(defun matrixmul (mat1 mat2 x)
    (if (= x 0) nil (append (list (helper2 (getrow (- (length mat1) x ) mat1)
     mat2 (length mat2))) (matrixmul mat1 mat2 (- x 1) ) ) )
)

(defun @ (mat1 mat2) (matrixmul mat1 mat2 (length mat1) ) )
