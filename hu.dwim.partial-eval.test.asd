;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.partial-eval.test
  :class hu.dwim.test-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.partial-eval"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.partial-eval)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "partial-eval" :depends-on ("suite"))))))
