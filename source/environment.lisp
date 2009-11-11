;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; Environment

(def special-variable *environment*)

(def class* environment ()
  ((assumptions nil :documentation "A list of forms that evaluate to #t in the current environment")
   (bindings nil :documentation "A list of alternating name value pairs, where name is a symbol and value is a walked-form")
   (types nil :documentation "A list of alternating name type pairs, where name is a symbol and type is a type designator")))

(def function clone-environment (&optional (environment *environment*))
  (make-instance 'environment
                 :assumptions (copy-seq (assumptions-of environment))
                 :bindings (copy-seq (bindings-of environment))
                 :types (copy-seq (types-of environment))))

;;;;;;
;;; Assumptions

(def function extend-assumptions (assumption)
  (partial-eval.debug "Extending assumptions with ~A" assumption)
  (push assumption (assumptions-of *environment*)))

;;;;;;
;;; Variable bindings

(def (function e) variable-binding (name)
  (assert (symbolp name))
  (aprog1 (getf (bindings-of *environment*) name nil)
    (partial-eval.debug "Retrieving variable binding ~A results in ~A" name it)))

(def function (setf variable-binding) (new-value name)
  (partial-eval.debug "Changing variable binding ~A to ~A" name new-value)
  (assert (symbolp name))
  (setf (getf (bindings-of *environment*) name) new-value))

(def function extend-bindings (bindings)
  (dolist (binding bindings)
    (setf (variable-binding (name-of binding)) (initial-value-of binding))))

;;;;;;
;;; Variable types

(def (function e) variable-type (name)
  (assert (symbolp name))
  (aprog1 (getf (types-of *environment*) name t)
    (partial-eval.debug "Retrieving variable type ~A results in ~A" name it)))

(def function (setf variable-type) (new-value name)
  (partial-eval.debug "Changing variable type ~A to ~A" name new-value)
  (assert (symbolp name))
  (setf (getf (types-of *environment*) name) new-value))
