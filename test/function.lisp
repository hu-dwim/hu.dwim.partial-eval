;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; function

(def suite* (test/function :in test))

;;;;;;
;;; list

(def test test/function/list ()
  (is (equal (partial-eval '(list)) nil))
  (is (equal (partial-eval '(list 1)) '(list 1)))
  (is (equal (partial-eval '(car (list 1 2))) 1))
  (is (equal (partial-eval '(car (list (cons 1 2)))) '(cons 1 2)))
  (is (equal (partial-eval '(cdr (list 1 2))) '(list 2)))
  (is (equal (partial-eval '(cdr (list 1))) nil)))

;;;;;;
;;; funcall

(def test test/function/funcall ()
  (is (equal (partial-eval '(funcall '+ 1 2)) 3))
  (is (equal (partial-eval '(funcall (lambda (&rest args) 1) nil)) 1)))

;;;;;;
;;; apply

(def test test/function/apply ()
  (is (equal (partial-eval '(apply (lambda (&rest args) 1) nil)) 1)))
