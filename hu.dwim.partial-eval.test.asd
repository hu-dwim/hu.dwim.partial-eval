;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.partial-eval.test
  :class hu.dwim.test-system
  :setup-readtable-function-name "hu.dwim.partial-eval::setup-readtable"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.partial-eval"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.stefil+swank
               :hu.dwim.partial-eval)
  :components ((:module "test"
                :components ((:file "generic-function" :depends-on ("suite"))
                             (:file "function" :depends-on ("suite"))
                             (:file "integer-power" :depends-on ("suite"))
                             (:file "list-append" :depends-on ("suite"))
                             (:file "make-instance" :depends-on ("suite"))
                             (:file "match-simple-regexp" :depends-on ("suite"))
                             (:file "package")
                             (:file "special-form" :depends-on ("suite"))
                             (:file "string-compare" :depends-on ("suite"))
                             (:file "suite" :depends-on ("package"))))))
