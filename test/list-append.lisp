;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; list-append-with-recursion

(def suite* (test/list-append-with-recursion :in test))

(def function list-append-with-recursion (list-1 list-2)
  "A recursive variant for list append."
  (if (consp list-1)
      (cons (car list-1)
            (list-append-with-recursion (cdr list-1) list-2))
      list-2))

;;;;;;
;;; correctness

(def test test/list-append-with-recursion/correctness ()
  (is (equal nil (list-append-with-recursion nil nil)))
  (is (equal '(1 2 3) (list-append-with-recursion '(1 2 3) nil)))
  (is (equal '(1 2 3) (list-append-with-recursion nil '(1 2 3))))
  (is (equal '(1 2 3 4 5 6) (list-append-with-recursion '(1 2 3) '(4 5 6)))))

;;;;;;
;;; partial-eval

(def layer list-append-with-recursion-layer (standard-partial-eval-layer)
  ())

(def layered-method inline-function-call? :in list-append-with-recursion-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(list-append-with-recursion))))

(def test test/list-append-with-recursion/partial-eval ()
  (with-active-layers (list-append-with-recursion-layer)
    (is (equal (partial-eval '(list-append-with-recursion '(1 2 3) list))
               '(cons 1 (cons 2 (cons 3 list)))))
    #+nil
    (is (equal (partial-eval '(list-append-with-recursion list '(1 2 3)))
               nil))))
