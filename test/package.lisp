;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval)

(defpackage :cl-partial-eval-test
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :iterate
        :stefil
        :defclass-star
        :cl-def
        :cl-syntax-sugar
        :cl-partial-eval)

  (:export #:test))
