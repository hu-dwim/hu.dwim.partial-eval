;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.partial-eval.test
  (:use :contextl
        :hu.dwim.common-lisp
        :hu.dwim.defclass-star
        :hu.dwim.def
        :hu.dwim.partial-eval
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.walker))
