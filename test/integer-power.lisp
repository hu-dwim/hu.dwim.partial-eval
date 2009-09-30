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
  "The usual power function with integer EXPONENT."
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

(def layer integer-power-with-loop ()
  ())

(def layered-method eval-function-call? :in integer-power-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(<= - ceiling))))

(def layered-method inline-function-call? :in integer-power-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(integer-power-with-loop))))

(def test test/integer-power-with-loop/partial-eval ()
  (is (equal (partial-eval '(integer-power-with-loop base 4) :layer 'integer-power-with-loop)
             '(* base (* base (* base (* base 1)))))))
