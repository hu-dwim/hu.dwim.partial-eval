;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; integer-power-with-loop

(def suite* (test/integer-power-with-loop :in test))

(def function integer-power-with-loop (base exponent)
  "A simple power function with non-negative integer EXPONENT."
  (loop
     :with result = 1
     :repeat exponent
     :do (setf result (* base result))
     :finally (return result)))

;;;;;;
;;; correctness

(def test test/integer-power-with-loop/correctness ()
  (is (= 1 (integer-power-with-loop 10 0)))
  (is (= 10 (integer-power-with-loop 10 1)))
  (is (= 100 (integer-power-with-loop 10 2)))
  (is (= 0 (integer-power-with-loop 0 1)))
  (is (= 0 (integer-power-with-loop 0 10))))

;;;;;;
;;; partial-eval

(def layer integer-power-with-loop-layer (standard-partial-eval-layer)
  ())

(def layered-method eval-function-call? :in integer-power-with-loop-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator '(ceiling))))

(def layered-method inline-function-call? :in integer-power-with-loop-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator '(integer-power-with-loop))))

(def test test/integer-power-with-loop/partial-eval ()
  (with-active-layers (integer-power-with-loop-layer)
    (is (equal 1 (partial-eval '(integer-power-with-loop base 0))))
    (is (equal (partial-eval '(integer-power-with-loop base 1))
               '(* base 1)))
    (is (equal (partial-eval '(integer-power-with-loop base 4))
               '(* base (* base (* base (* base 1))))))
    (is (equal 8
               (eval `(bind ((base 2)
                             (exponent 3))
                        ,(partial-eval '(integer-power-with-loop 2 exponent))))))
    (is (equal 8
               (eval `(bind ((base 2)
                             (exponent 3))
                        ,(partial-eval '(integer-power-with-loop base exponent))))))))
