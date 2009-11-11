;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; Util

(def function make-progn-form (body)
  (cond ((null body)
         (make-instance 'constant-form :value nil))
        ((length= body 1)
         (first body))
        (t
         (make-instance 'progn-form
                        :body (iter (for form :in body)
                                    (if (typep form 'progn-form)
                                        (appending (body-of form))
                                        (collect form)))))))

(def function make-free-application-form (operator arguments)
  (if (consp operator)
      (make-instance 'free-application-form
                     :operator 'funcall
                     :arguments (cons (make-instance 'function-object-form :name operator) arguments))
      (make-instance 'free-application-form
                     :operator operator
                     :arguments arguments)))

(def function variable-referenced? (name ast)
  (map-ast (lambda (ast)
             ;; FIXME: this is most probably broken with special-variables, and their handling might even need some walker updates
             (when (and (typep ast 'variable-reference-form)
                        (eq name (name-of ast)))
               (return-from variable-referenced? #t))
             ast)
           ast)
  #f)

(def function block-referenced? (block ast)
  (map-ast (lambda (ast)
             (when (and (typep ast 'return-from-form)
                        (eq block (target-block-of ast)))
               (return-from block-referenced? #t))
             ast)
           ast)
  #f)

(def function constant-values? (forms)
  (every (of-type '(or constant-form free-function-object-form)) forms))

(def function constant-values (forms)
  (mapcar (lambda (form)
            (etypecase form
              (constant-form (value-of form))
              (free-function-object-form (name-of form))))
          forms))

(def function current-layer-prototype ()
  (contextl::layer-context-prototype (current-layer-context)))
