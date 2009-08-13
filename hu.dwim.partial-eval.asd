;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.partial-eval
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Extensible partial evaluator"
  :depends-on (#+nil :cl-unification
               :hu.dwim.common-lisp
               :hu.dwim.def
               :hu.dwim.def+hu.dwim.logger
               :hu.dwim.defclass-star
               :hu.dwim.syntax-sugar
               :hu.dwim.util
               :hu.dwim.walker
               :swank)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "duplicates" :depends-on ("package"))
                             (:file "configuration" :depends-on ("duplicates"))
                             (:file "source" :depends-on ("configuration"))
                             (:file "partial-eval" :depends-on ("source"))))))
