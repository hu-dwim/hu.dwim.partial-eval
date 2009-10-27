;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; Generic function

(def suite* (test/generic-function :in test))

(def generic print-applied-method-information (instance))

;;;;;;
;;; t

(def method print-applied-method-information :before (instance)
  (print "t: before"))

(def method print-applied-method-information :around (instance)
  (print "t: enter around")
  (call-next-method)
  (print "t: leave around"))

(def method print-applied-method-information (instance)
  (print "t: primary"))

(def method print-applied-method-information :after (instance)
  (print "t: after"))

;;;;;;
;;; string

(def method print-applied-method-information :before ((instance string))
  (print "string: before"))

(def method print-applied-method-information :around ((instance string))
  (print "string: enter around")
  (call-next-method)
  (print "string: leave around"))

(def method print-applied-method-information ((instance string))
  (print "string: primary enter")
  (call-next-method)
  (print "string: primary leave"))

(def method print-applied-method-information :after ((instance string))
  (print "string: after"))

;;;;;;
;;; integer

(def method print-applied-method-information :before ((instance integer))
  (print "integer: before"))

(def method print-applied-method-information :around ((instance integer))
  (print "integer: enter around")
  (call-next-method)
  (print "integer: leave around"))

(def method print-applied-method-information ((instance integer))
  (print "integer: primary enter")
  (call-next-method)
  (print "integer: primary leave"))

(def method print-applied-method-information :after ((instance integer))
  (print "integer: after"))

;;;;;;
;;; partial-eval

(def layer generic-function-layer (standard-partial-eval-layer)
  ())

(def layered-method eval-function-call? :in generic-function-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator '(typep list* ; TODO: eliminate these
                         sb-int:proper-list-of-length-p))))

(def layered-method inline-function-call? :in generic-function-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator
              '(print-applied-method-information))))

(def test test/generic-function/partial-eval ()
  (with-active-layers (generic-function-layer)
    (is (equal (partial-eval '(print-applied-method-information t))
               nil))
    (is (equal (partial-eval '(print-applied-method-information 42))
               nil))
    (is (equal (partial-eval '(print-applied-method-information "42"))
               nil))))
