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






















#|
(LAMBDA (INSTANCE)
  (BIND ((#:ARGUMENTS-LIST1674 (LIST* INSTANCE NIL)))
    (FLET ((#:METHOD1675 (#:ARGUMENTS1677 #:METHODS1678)
             (FLET ((CALL-NEXT-METHOD ()
                      (APPLY (FIRST #:METHODS1678) #:ARGUMENTS1677)))
               (BIND (((INSTANCE) #:ARGUMENTS1677))
                 (PRINT "t: primary"))))
           (#:METHOD1676 (#:ARGUMENTS1679 #:METHODS1680)
             (FLET ((CALL-NEXT-METHOD ()
                      (APPLY (FIRST #:METHODS1680) #:ARGUMENTS1679)))
               (BIND (((INSTANCE) #:ARGUMENTS1679))
                 (PRINT "t: before")))))
      (MACROLET ((SB-PCL::METHODS (METHODS)
                   (BIND ((METHOD (FIRST METHODS))
                          (HU.DWIM.PARTIAL-EVAL::NAME
                           (SECOND
                            (FIND METHOD
                                  '((#<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION (T) {10068F50C1}>
                                     #:METHOD1675)
                                    (#<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION :BEFORE (T) {10068F50E1}>
                                     #:METHOD1676))
                                  :KEY 'CAR)))
                          (HU.DWIM.PARTIAL-EVAL::ARGUMENTS '(INSTANCE)))
                     (IF (NULL METHOD)
                         (IF (NOT 'NIL)
                             (LIST* 'NO-APPLICABLE-METHOD
                                    '#<STANDARD-GENERIC-FUNCTION PRINT-APPLIED-METHOD-INFORMATION (2)>
                                    HU.DWIM.PARTIAL-EVAL::ARGUMENTS)
                             (LIST* 'APPLY ''NO-APPLICABLE-METHOD
                                    '#<STANDARD-GENERIC-FUNCTION PRINT-APPLIED-METHOD-INFORMATION (2)>
                                    (APPEND
                                     HU.DWIM.PARTIAL-EVAL::ARGUMENTS
                                     '(HU.DWIM.PARTIAL-EVAL::.REST.))))
                         `(,HU.DWIM.PARTIAL-EVAL::NAME
                           #:ARGUMENTS-LIST1674
                           ',(MAPCAR
                              (LAMBDA (METHOD)
                                (SECOND
                                 (FIND METHOD
                                       '((#<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION (T) {10068F50C1}>
                                          #:METHOD1675)
                                         (#<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION :BEFORE (T) {10068F50E1}>
                                          #:METHOD1676))
                                       :KEY 'CAR)))
                              (REST METHODS)))))))
        (SB-PCL::METHODS
         (#<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION (T) {10068F50C1}>
          #<STANDARD-METHOD PRINT-APPLIED-METHOD-INFORMATION :BEFORE (T) {10068F50E1}>))))))
|#