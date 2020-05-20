;!=== Lathe.lisp Machine-Learning Library ===!
;|    = Easily ML Lathe.preprocess.lisp =    |
;|      Lathe Software Foundation 2020       |
;|          MIT Permissive License           |
(defpackage :preprocess
   (:use :common-lisp)
   (:export :StandardScalar)
)
(in-package :preprocess)
(load "stats.lisp")
(use-package 'stats)
(defun StandardScalar (x)
  (
  (setq μ (mean x))
  (setq σ (std x))
  (loop for z in x
  collect ((/ (- z μ) σ)))
  ))
