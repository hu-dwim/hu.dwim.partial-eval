;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.partial-eval
  (:use :contextl
        :hu.dwim.asdf
        :hu.dwim.common-lisp
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.walker))

(in-package :hu.dwim.partial-eval)

(def logger partial-eval ()
  :level (if *load-as-production?* +info+ +debug+)
  :compile-time-level (if *load-as-production?* +debug+ +dribble+)
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))
