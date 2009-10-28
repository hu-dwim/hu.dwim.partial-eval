;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; Function

(def suite* (test/function :in test))

;;;;;;
;;; partial-eval

(def layer function-layer (standard-partial-eval-layer)
  ())

(def test test/function/partial-eval ()
  (with-active-layers (function-layer)
    (is (equal (partial-eval '(funcall '+ 1 2)) 3))))
