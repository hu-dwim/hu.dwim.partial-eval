;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; standard-class-without-slots

(def suite* (test/standard-class-without-slots :in test))

(def class standard-class-without-slots ()
  ())

;;;;;;
;;; partial-eval

(def layer standard-class-without-slots-layer (standard-partial-eval-layer)
  ())

(def layered-method inline-function-call? :in standard-class-without-slots-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(standard-class-without-slots))))

(def test test/standard-class-without-slots/partial-eval ()
  #+nil
  (with-active-layers (standard-class-without-slots-layer)
    (is (equal (partial-eval '(make-instance initialize-instance allocate-instance))
               nil))))

;;;;;;
;;; standard-class-with-slots

(def suite* (test/standard-class-with-slots :in test))

(def class* standard-class-with-slots ()
  ((foo 1)
   (bar 2)))

;;;;;;
;;; partial-eval

(def layer standard-class-with-slots-layer (standard-partial-eval-layer)
  ())

(def layered-method inline-function-call? :in standard-class-with-slots-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(make-instance initialize-instance allocate-instance))))

(def test test/standard-class-with-slots/partial-eval ()
  #+nil
  (with-active-layers (standard-class-with-slots-layer)
    (is (equal (partial-eval '(make-instance 'standard-class-with-slots))
               nil))))
