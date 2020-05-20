;!=== Lathe.lisp Machine-Learning Library ===!
;|      = Easily ML Lathe.stats.lisp =       |
;|      Lathe Software Foundation 2020       |
;|          MIT Permissive License           |
; ~~~~ Make Package ~~~~
(defpackage :stats
   (:use :common-lisp)
   (:export :.* :sum :mean :variance :std :ste :r :r2 :independent_t
   :f_test :gets :getrow :getcol :dot)
)
; ================ Stats ================__
(in-package :stats)
; Dot (A lot easier)
(defun .* (x y)(mapcar '* x y)
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
(defun r (x y) (setq n (length x))
(setq yn (length y)) (setq Σx (sum x)) (setq Σy (sum y))
 (setq Σx2 (sum (.* x x))) (setq Σy2 (sum (.* y y)))
 (setq Σxy (sum (.* x y)))
 ()
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

(defmacro dot (mat1 mat2) (matrixmul mat1 mat2 (length mat1) ) )
; ============================================
