;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

(defpackage :hu.dwim.partial-eval.test
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :defclass-star
        :cl-def
        :cl-syntax-sugar
        :hu.dwim.partial-eval)

  (:export #:test))
