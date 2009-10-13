;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.partial-eval
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Extensible partial evaluator"
  :depends-on (:hu.dwim.common-lisp
               :hu.dwim.def+contextl
               :hu.dwim.defclass-star
               :hu.dwim.logger
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :hu.dwim.walker
               :swank)
  :components ((:module "source"
                :components ((:file "configuration" :depends-on ("package"))
                             (:file "logger" :depends-on ("configuration"))
                             (:file "source" :depends-on ("configuration"))
                             (:file "package")
                             (:file "partial-eval" :depends-on ("source" "logger"))))))
