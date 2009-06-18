;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(cl:in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(defpackage #:cl-partial-eval-system
  (:use :cl :asdf :cl-syntax-sugar)

  (:export #:*load-as-production-p*
           #:project-relative-pathname
           ))

(in-package #:cl-partial-eval-system)

(defvar *load-as-production-p* t)

(defun project-relative-pathname (path)
  (merge-pathnames path (component-pathname (find-system :cl-partial-eval))))

(defsystem :cl-partial-eval
  :version "0.1"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-partial-eval::setup-readtable"
  :depends-on (:metabang-bind
               :alexandria
               :anaphora
               :iterate
               :defclass-star
               :closer-mop
               :cl-def
               :cl-syntax-sugar
               :cl-walker
               :cl-yalog
               #+nil :cl-unification)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "duplicates" :depends-on ("package"))
             (:file "configuration" :depends-on ("duplicates"))
             (:file "partial-eval" :depends-on ("configuration"))))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-partial-eval))))
  (operate 'load-op :cl-partial-eval-test)
  (in-package :cl-partial-eval-test)
  (eval (read-from-string "(progn
                             (stefil:funcall-test-with-feedback-message 'test))"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-partial-eval))))
  nil)
