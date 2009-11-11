;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; test/integer-power

(def suite* (test/integer-power :in test))

(def function integer-power (base exponent)
  "A simple power function with non-negative integer EXPONENT."
  (loop
     :with result = 1
     :repeat exponent
     :do (setf result (* base result))
     :finally (return result)))

(def test test/integer-power/correctness ()
  (is (= 1 (integer-power 10 0)))
  (is (= 10 (integer-power 10 1)))
  (is (= 100 (integer-power 10 2)))
  (is (= 0 (integer-power 0 1)))
  (is (= 0 (integer-power 0 10))))

(def function partial-eval/integer-power (form)
  (partial-eval form
                :eval-function-calls '(ceiling)
                :inline-function-calls '(integer-power)))

(def test test/integer-power/partial-eval ()
  (is (equal (partial-eval/integer-power '(integer-power base 0))
             1))
  (is (equal (partial-eval/integer-power '(integer-power base 1))
             '(* base 1)))
  (is (equal (partial-eval/integer-power '(integer-power base 4))
             '(* base (* base (* base (* base 1)))))))
