;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :hu.dwim.partial-eval
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :anaphora
        :iterate
        :defclass-star
        :closer-mop
        #+nil :unify
        :cl-def
        :cl-syntax-sugar
        :cl-walker
        :cl-yalog
        :hu.dwim.util
        :hu.dwim.partial-eval.system)

  (:shadow #:environment
           #:make-empty-environment))

(in-package :hu.dwim.partial-eval)

(def logger partial-eval ()
  :level (if *load-as-production-p* +info+ +debug+)
  :compile-time-level (if *load-as-production-p* +debug+ +dribble+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
