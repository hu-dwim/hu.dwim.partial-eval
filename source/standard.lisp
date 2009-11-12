;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; standard-partial-eval-layer

(def (layer e) standard-partial-eval-layer (partial-eval-layer)
  ())

(def layered-method eval-function-call? :in standard-partial-eval-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator '(eq eql not null endp atom car cdr consp first second third fourth length getf char= stringp symbolp
                         integerp zerop plusp minusp < <= = >= > - + * / 1+ 1-))))

;;;;;;
;;; function-call-return-value

(def layered-method function-call-return-value :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'not)) arguments)
  (bind ((result (return-value (first (arguments-of ast)))))
    (if (typep result 'constant-form)
        (make-instance 'constant-form :value (not (value-of result)))
        (make-instance 'free-application-form
                       :operator 'not
                       :arguments (list result)))))

;;;;;;
;;; partial-eval-function-call

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'values)) arguments)
  (if (length= 1 arguments)
      (first-elt arguments)
      (call-next-layered-method)))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'apply)) arguments)
  (bind ((function (first arguments))
         (last-argument (last-elt arguments)))
    (if (or (typep last-argument 'constant-form)
            (and (typep last-argument 'free-application-form)
                 (eq 'list (operator-of last-argument))))
        (bind ((arguments (append (butlast (rest arguments))
                                  (etypecase last-argument
                                    (constant-form
                                     (mapcar (lambda (value)
                                               (make-instance 'constant-form :value value))
                                             (value-of last-argument)))
                                    (free-application-form
                                     (arguments-of last-argument))))))
          (typecase function
            (constant-form
             (partial-eval (make-instance 'free-application-form
                                          :operator (value-of function)
                                          :arguments arguments)))
            (free-function-object-form
             (partial-eval-form (make-instance 'free-application-form
                                               :operator (name-of function)
                                               :arguments arguments)))
            (lambda-function-form
             (partial-eval-form (make-instance 'lambda-application-form
                                               :operator function
                                               :arguments arguments)))
            (t (call-next-layered-method))))
        (call-next-layered-method))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'funcall)) arguments)
  (bind ((function (first arguments)))
    (typecase function
      (constant-form
       (partial-eval (make-instance 'free-application-form
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
       (partial-eval-form (make-instance 'free-application-form
                                         :operator (name-of function)
                                         :arguments (rest arguments))))
      (lambda-function-form
       (partial-eval-form (make-instance 'lambda-application-form
                                         :operator function
                                         :arguments (cdr arguments))))
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
  (bind ((first-argument (first arguments)))
    (cond ((and (length= 1 arguments)
                (or (typep first-argument 'constant-form)
                    (and (typep first-argument 'free-application-form)
                         (eq 'list (operator-of first-argument)))))
           first-argument)
          (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'null)) arguments)
  (bind ((argument (first arguments)))
    (cond ((and (typep argument 'free-application-form)
                (eq 'list (operator-of argument))
                (> (length (arguments-of argument)) 0))
           (make-instance 'constant-form :value #f))
          ((and (typep argument 'free-application-form)
                (eq 'list* (operator-of argument))
                (> (length (arguments-of argument)) 1))
           (make-instance 'constant-form :value #f))
          (t (call-next-layered-method)))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'consp)) arguments)
     (bind ((argument (first arguments)))
       (cond ((and (typep argument 'free-application-form)
                   (eq 'list* (operator-of argument))
                   (> (length (arguments-of argument)) 1))
              (make-instance 'constant-form :value #t))
             (t (call-next-layered-method)))))

(def function have-common-subclass? (class-1 class-2)
  (labels ((subclasses (class)
             (bind ((direct-subclasses (class-direct-subclasses class)))
               (append direct-subclasses
                       (mappend #'subclasses direct-subclasses)))))
    (intersection (subclasses class-1)
                  (subclasses class-2))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'typep)) arguments)
  (bind ((value-argument (first arguments))
         (value-type (return-type value-argument))
         (type-argument (second arguments)))
    (if (typep type-argument 'constant-form)
        (cond ((subtypep value-type (value-of type-argument))
               (make-instance 'constant-form :value #t))
              ((subtypep `(and ,value-type
                               ,(value-of type-argument)) nil)
               (make-instance 'constant-form :value #f))
              ((and (find-class value-type nil)
                    (find-class (value-of type-argument) nil))
               (if (have-common-subclass? (find-class value-type nil)
                                          (find-class (value-of type-argument) nil))
                   (call-next-layered-method)
                   (make-instance 'constant-form :value #f)))
              (t (call-next-layered-method)))
        (call-next-layered-method))))

(def layered-method partial-eval-function-call :in standard-partial-eval-layer ((ast free-application-form) (operator (eql 'class-of)) arguments)
  (bind ((argument (first arguments)))
    (if (and (typep argument 'variable-reference-form)
             (variable-type (name-of argument)))
        (make-instance 'constant-form :value (find-class (variable-type (name-of argument))))
        (call-next-layered-method))))
