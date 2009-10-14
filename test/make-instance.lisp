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

(def layered-method eval-function-call? :in standard-class-without-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator
              '(list rplacd list* ; TODO: eliminate this three
                typep find-class class-finalized-p finalize-inheritance class-default-initargs class-of class-slots
                sb-int:list-of-length-at-least-p sb-pcl::allocate-standard-instance sb-pcl::get-instance-hash-code
                sb-pcl::class-wrapper sb-kernel:layout-length sb-kernel::classoid-of))))

(def layered-method inline-function-call? :in standard-class-without-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-method)
      (member operator
              '(make-instance allocate-instance initialize-instance shared-initialize))))

(def layered-method lookup-variable-value? :in standard-class-without-slots-layer ((name (eql 'sb-pcl::**boot-state**)))
  #t)

(def test test/standard-class-without-slots/partial-eval ()
  (with-active-layers (standard-class-without-slots-layer)
    (is (equal (partial-eval '(make-instance 'standard-class-without-slots))
               nil))))

;;;;;;
;;; standard-class-with-slots

(def suite* (test/standard-class-with-slots :in test))

(def class* standard-class-with-slots ()
  ((foo 1)
   (bar 2)))

;;;;;;
;;; partial-eval

(def layer standard-class-with-slots-layer (standard-class-without-slots-layer)
  ())

(def test test/standard-class-with-slots/partial-eval ()
  (with-active-layers (standard-class-with-slots-layer)
    (is (equal (partial-eval '(make-instance 'standard-class-with-slots))
               nil))))
