;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; standard-partial-eval-layer

(def (layer e) standard-partial-eval-layer ()
  ())

(def layered-method eval-function-call? :in standard-partial-eval-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator '(eq eql not null endp atom car cdr consp first second third fourth length getf char= zerop < <= = >= > - + * / 1+ 1-))))


;;;;;;
;;; partial-eval-function-call

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'apply)) arguments)
  (if (and (typep (first arguments) 'free-function-object-form)
           (typep (last-elt arguments) 'constant-form))
      (partial-eval-form (make-instance 'free-application-form
                                        :operator (name-of (first arguments))
                                        :arguments (append (rest (butlast arguments))
                                                           (mapcar (lambda (value)
                                                                     (make-instance 'constant-form :value value))
                                                                   (value-of (last-elt arguments))))))
      (call-next-layered-method)))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'funcall)) arguments)
  (bind ((function (first arguments)))
    (typecase function
      (constant-form
       (partial-eval-form (make-instance 'free-application-form
                                         :operator (value-of function)
                                         :arguments (rest arguments))))
      (lexical-function-object-form
       (partial-eval-form (make-instance 'lexical-application-form
                                         :operator (name-of function)
                                         :definition (make-instance 'lambda-function-form
                                                                    :body (body-of (definition-of function))
                                                                    :arguments (arguments-of (definition-of function)))
                                         :arguments (rest arguments))))
      (function-object-form
       (partial-eval-form (make-instance 'lexical-application-form
                                         :operator (name-of function)
                                         :definition function
                                         :arguments (rest arguments))))
      (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'car)) arguments)
  (bind ((argument (first arguments)))
    (cond ((and (typep argument 'free-application-form)
                (eq 'list (operator-of argument)))
           (partial-eval-form (first (arguments-of argument))))
          ((and (typep argument 'free-application-form)
                (eq 'list* (operator-of argument))
                (> (length (arguments-of argument)) 1))
           (partial-eval-form (first (arguments-of argument))))
          (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'cdr)) arguments)
  (bind ((argument (first arguments)))
    (cond ((and (typep argument 'free-application-form)
                (eq 'list (operator-of argument)))
           (partial-eval-form (make-instance 'free-application-form
                                             :operator 'list
                                             :arguments (cdr (arguments-of argument)))))
          ((and (typep argument 'free-application-form)
                (eq 'list* (operator-of argument))
                (> (length (arguments-of argument)) 1))
           (partial-eval-form (make-instance 'free-application-form
                                             :operator 'list*
                                             :arguments (cdr (arguments-of argument)))))
          (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'list)) arguments)
  (if arguments
      (call-next-layered-method)
      (make-instance 'constant-form :value nil)))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'list*)) arguments)
  (if (and (length= 1 arguments)
           (typep (first arguments) 'constant-form))
      (first arguments)
      (call-next-layered-method)))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'consp)) arguments)
  (bind ((argument (first arguments)))
    (cond ((and (typep argument 'free-application-form)
                (eq 'list* (operator-of argument))
                (> (length (arguments-of argument)) 1))
           (make-instance 'constant-form :value #t))
          (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'typep)) arguments)
  (bind ((argument (first arguments)))
    (if (and (typep argument 'variable-reference-form)
             (variable-type (name-of argument)))
        (partial-eval-form (make-instance 'free-application-form
                                          :operator 'subtypep
                                          :arguments (list (make-instance 'constant-form :value (variable-type (name-of argument)))
                                                           (second arguments))))
        (call-next-layered-method))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'class-of)) arguments)
  (bind ((argument (first arguments)))
    (if (and (typep argument 'variable-reference-form)
             (variable-type (name-of argument)))
        (make-instance 'constant-form :value (find-class (variable-type (name-of argument))))
        (call-next-layered-method))))
