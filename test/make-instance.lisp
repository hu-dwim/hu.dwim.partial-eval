;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; test/make-instance

(def suite* (test/make-instance :in test))

;;;;;;
;;; test/make-instance/without-slots

(def suite* (test/make-instance/without-slots :in test/make-instance))

(def layer make-instance/without-slots-layer (standard-partial-eval-layer)
  ())

(def layered-method eval-function-call? :in make-instance/without-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator
              '(list rplacd list* ; TODO: eliminate these
                typep subtypep find-class class-finalized-p finalize-inheritance class-default-initargs class-of class-slots class-prototype
                sb-int:list-of-length-at-least-p sb-pcl::class-wrapper sb-kernel:layout-length sb-kernel::classoid-of))))

(def layered-method inline-function-call? :in make-instance/without-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator
              '(make-instance allocate-instance initialize-instance shared-initialize
                sb-int:list-of-length-at-least-p sb-pcl::allocate-standard-instance sb-pcl::get-instance-hash-code))))

(def layered-method lookup-variable-value? :in make-instance/without-slots-layer ((ast free-variable-reference-form) (name (eql 'sb-pcl::**boot-state**)))
  #t)

(def layered-method partial-eval-function-call :in make-instance/without-slots-layer ((ast free-application-form) (operator (eql 'sb-kernel::classoid-of)) arguments)
  (bind ((argument (first arguments)))
    (if (and (typep argument 'variable-reference-form)
             (variable-type (name-of argument)))
        (make-instance 'constant-form :value (sb-kernel:find-classoid (hu.dwim.partial-eval::variable-type (name-of argument))))
        (call-next-layered-method))))

(def test test/make-instance/without-slots/partial-eval ()
  (with-active-layers (make-instance/without-slots-layer)
    (is (equal (partial-eval '(make-instance 'standard-class/without-slots) :types '(sb-kernel::instance 'integer))
               nil))))

;;;;;;
;;; standard-class/with-slots

(def suite* (test/make-instance/with-slots :in test/make-instance))

;;;;;;
;;; partial-eval

(def layer make-instance/with-slots-layer (make-instance/without-slots-layer slot-value-using-class-layer)
  ())

(def layered-method eval-function-call? :in make-instance/with-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator
              '(slot-definition-location slot-definition-initfunction slot-definition-initargs
                sb-int::memq sb-pcl::safe-p sb-pcl::std-instance-p sb-int:proper-list-of-length-p sb-pcl::check-obsolete-instance
                sb-pcl::clos-slots-ref sb-kernel:%instance-ref sb-pcl::check-initargs-1))
      (typep operator 'function)))

(def layered-method partial-eval-function-call :in make-instance/with-slots-layer ((ast free-application-form) (operator (eql 'eq)) arguments)
  ;; KLUDGE: to avoid checking for slots being unbound
  (if (and (typep (second arguments) 'special-variable-reference-form)
           (eq (name-of (second arguments)) 'sb-pcl::+slot-unbound+))
      (make-instance 'constant-form :value #t)
      (call-next-layered-method)))

(def layered-method inline-function-call? :in make-instance/with-slots-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator '((setf slot-value-using-class) slot-boundp-using-class
                         (setf sb-pcl::clos-slots-ref) sb-int:proper-list-of-length-p) :test #'equal)))

(def test test/make-instance/with-slots/partial-eval ()
  (with-active-layers (make-instance/with-slots-layer)
    (is (equal (partial-eval '(make-instance 'standard-class/with-slots))
               nil))))
