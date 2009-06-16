;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-partial-eval)
  (asdf:oos 'asdf:load-op :cl-syntax-sugar))

(in-package :cl-partial-eval-system)

(setf *load-as-production-p* nil)

(defsystem :cl-partial-eval-test
  :description "Tests for cl-partial-eval."
  :default-component-class cl-source-file-with-readtable
  :class system-with-readtable
  :setup-readtable-function "cl-partial-eval::setup-readtable"
  :depends-on (:metabang-bind
               :iterate
               :stefil
               :cl-def
               :cl-syntax-sugar
               :cl-partial-eval)
  :components
  ((:module :test
	    :components
            ((:file "package")
             (:file "suite" :depends-on ("package"))
             (:file "match" :depends-on ("suite"))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-partial-eval-test))))
  (in-package :cl-partial-eval-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features* and (declaim (optimize (debug 3))) was issued to help later C-c C-c'ing"))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-partial-eval-test))))
  nil)
