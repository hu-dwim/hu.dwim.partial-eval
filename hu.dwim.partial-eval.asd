;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(defpackage #:hu.dwim.partial-eval.system
  (:use :cl :asdf :cl-syntax-sugar)

  (:export #:*load-as-production-p*
           #:project-relative-pathname
           ))

(in-package #:hu.dwim.partial-eval.system)

(defvar *load-as-production-p* t)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :hu.dwim.partial-eval))))

(defsystem :hu.dwim.partial-eval
  :version "0.1"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "hu.dwim.partial-eval::setup-readtable"
  :depends-on (:metabang-bind
               :alexandria
               :anaphora
               :iterate
               :defclass-star
               :swank
               :closer-mop
               :cl-def
               :cl-syntax-sugar
               :cl-walker
               :cl-yalog
               #+nil :cl-unification
               :hu.dwim.util)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "duplicates" :depends-on ("package"))
                 (:file "configuration" :depends-on ("duplicates"))
                 (:file "source" :depends-on ("configuration"))
                 (:file "partial-eval" :depends-on ("source"))))))

(defmethod perform ((op test-op) (system (eql (find-system :hu.dwim.partial-eval))))
  (operate 'load-op :hu.dwim.partial-eval.test)
  (in-package :hu.dwim.partial-eval.test)
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :hu.dwim.partial-eval))))
  nil)
