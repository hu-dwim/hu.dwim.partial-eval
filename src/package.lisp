;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-partial-eval
  (:use :common-lisp
        :metabang-bind
        :alexandria
        :anaphora
        :iterate
        :defclass-star
        :closer-mop
        :cl-def
        :cl-syntax-sugar
        :cl-walker
        :cl-yalog
        :cl-partial-eval-system))

(in-package :cl-partial-eval)

(def logger partial-eval ()
  :level (if *load-as-production-p* +info+ +debug+)
  :compile-time-level (if *load-as-production-p* +debug+ +dribble+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
