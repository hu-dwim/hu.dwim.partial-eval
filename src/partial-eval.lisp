;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval)

;;;;;;
;;; Function with source

(def (definer e) function-with-soucre (name args documentation &body forms)
  `(progn
     (setf (function-source ',name) `(lambda ,',args ,',@forms))
     (def function ,name ,args ,documentation ,@forms)))

(def special-variable *function-sources* (make-hash-table))

(def function function-source (name)
  (gethash name *function-sources*))

(def function (setf function-source) (form name)
  (setf (gethash name *function-sources*) form))

;;;;;;
;;; Partial eval

(def special-variable *environment*)

(def class* environment ()
  ((assumptions nil)
   (bindings nil)))

(def function make-empty-environment ()
  (make-instance 'environment))

(def function clone-environment ()
  (format t "Cloning environment~%")
  (make-instance 'environment
                 :assumptions (assumptions-of *environment*)
                 :bindings (bindings-of *environment*)))

(def function primitive-operator? (operator)
  (member operator '(eq eql + - * / 1+ 1- = < <= > >= length elt floor ceiling round)))

(def function variable-binding (name)
  (assert (symbolp name))
  (aprog1 (getf (bindings-of *environment*) name :unbound)
    (format t "Retrieving variable binding ~A ~A~%" name it)))

(def function (setf variable-binding) (new-value name)
  (format t "Changing variable binding ~A ~A~%" name new-value)
  (assert (symbolp name))
  (setf (getf (bindings-of *environment*) name) new-value))

(def function extend-bindings (bindings)
  (dolist (binding bindings)
    (bind ((name (car binding))
           (value (cdr binding)))
      (setf (variable-binding name) value))))

(def function extend-assumptions (assumption)
  (format t "Extending assumptions ~A~%" assumption)
  (push assumption (assumptions-of *environment*)))

(def function has-side-effect? (form)
  (not (typep form '(or constant-form variable-reference-form))))

(def function variable-referenced? (name ast)
  #f)

(def function block-referenced? (name ast)
  #f)

(def function go-tag-referenced? (name ast)
  #f)

(def function  wrap-with-progn-form (forms)
  (if (length= forms 1)
      (first forms)
      (make-instance 'progn-form :body forms)))

(def function eval-bindings (bindings)
  (mapcar (lambda (binding)
            (bind ((name (car binding))
                   (value (partial-eval (cdr binding))))
              (cons name value)))
          bindings))

(def function eval-implicit-progn-body (ast)
  (iter (with forms = (if (listp ast)
                          ast
                          (body-of ast)))
        (with length = (length forms))
        (for index :from 0)
        (for form :in forms)
        (for evaluated-form = (partial-eval form))
        (when (or (eq index (1- length))
                  (has-side-effect? evaluated-form))
          (collect evaluated-form))))

(def (generic e) partial-eval (form)
  (:documentation "Handles constants, function application and special forms IF, PROGN, BLOCK, RETURN-FROM, TAGBODY, GO, LET, LET*, SETQ, FLET and LABELS.")

  (:method ((form list))
    (bind ((*environment* (make-empty-environment)))
      (unwalk-form (partial-eval (walk-form form)))))

  (:method ((ast constant-form))
    ast)

  (:method ((ast if-form))
    (flet ((eval-then ()
             (bind ((*environment* (clone-environment)))
               (extend-assumptions `(not (eq #f ,(unwalk-form (condition-of ast)))))
               (partial-eval (then-of ast))))
           (eval-else ()
             (bind ((*environment* (clone-environment)))
               (extend-assumptions `(eq #f ,(unwalk-form (condition-of ast))))
               (partial-eval (else-of ast)))))
      (bind ((condition (partial-eval (condition-of ast))))
        (format t "Checking condition ~A~%" condition)
        (if (typep condition 'constant-form)
            (if (value-of condition)
                (eval-then)
                (eval-else))
            (make-instance 'if-form
                           :condition condition
                           :then (eval-then)
                           :else (eval-else))))))
  (:method ((ast progn-form))
    (wrap-with-progn-form (eval-implicit-progn-body ast)))

  (:method ((ast block-form))
    (bind ((body (eval-implicit-progn-body ast)))
      ;; TODO
      (if (some (lambda (form)
                  (block-referenced? (name-of ast) form))
                body)
          (make-instance 'block-form
                         :name (name-of ast)
                         :body body)
          (wrap-with-progn-form body))))

  (:method ((ast return-from-form))
    ast)

  (:method ((ast tagbody-form))
    (iter (with body = (body-of ast))
          (catch ast
            (bind ((body (mapcar #'partial-eval (body-of ast)))
                   (runtime-body (remove-if (lambda (form)
                                              (and (typep form 'go-tag-form)
                                                   (not (go-tag-referenced? (name-of form) body))))
                                            body)))
              (if (some (of-type 'go-tag-form) runtime-body)
                  (make-instance 'tagbody-form
                                 :body runtime-body)
                  (wrap-with-progn-form runtime-body))))))

  (:method ((ast go-tag-form))
    ast)

  (:method ((ast go-form))
    (throw (enclosing-tagbody-of ast) ast))

  (:method ((ast setq-form))
    (break "~A" *environment*)
    (setf (variable-binding (name-of (variable-of ast))) (partial-eval (value-of ast))))

  (:method ((ast variable-binding-form))
    (bind ((let*-form? (typep ast 'let*-form))
           (*environment* (clone-environment))
           (bindings (mapcar (lambda (binding)
                               (bind ((name (car binding))
                                      (value (partial-eval (cdr binding))))
                                 (when let*-form?
                                   (setf (variable-binding name) value))
                                 (cons name value)))
                             (bindings-of ast))))
      (unless let*-form?
        (extend-bindings bindings))
      (bind ((body (eval-implicit-progn-body ast))
             (runtime-bindings (remove-if-not (lambda (binding)
                                                (some (lambda (form)
                                                        (bind ((name (car binding)))
                                                          (variable-referenced? name form)))
                                                      body))
                                              bindings)))
        (if runtime-bindings
            (make-instance (class-of ast)
                           :bindings runtime-bindings
                           :body body)
            (wrap-with-progn-form body)))))

  (:method ((ast lexical-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if (eq :unbound value)
          ast
          value)))

  (:method ((ast free-variable-reference-form))
    ast)

  (:method ((ast free-application-form))
    (bind ((operator (operator-of ast)))
      (if (primitive-operator? operator)
          (bind ((arguments (mapcar #'partial-eval (arguments-of ast))))
            (if (every (of-type 'constant-form) arguments)
                (make-instance 'constant-form
                               :value (apply operator (mapcar #'value-of arguments)))
                (make-instance 'free-application-form
                               :operator operator
                               :arguments arguments)))
          (bind ((*environment* (clone-environment))
                 (lambda-ast (walk-form (aprog1 (function-source operator)
                                          (assert it nil "Source not found for ~A" operator)))))
            (extend-bindings (eval-bindings (mapcar #'cons
                                                    (mapcar #'name-of (arguments-of lambda-ast))
                                                    (arguments-of ast))))
            (wrap-with-progn-form (eval-implicit-progn-body lambda-ast))))))

  (:method ((ast flet-form))
    (eval-implicit-progn-body ast))

  (:method ((ast labels-form))
    (eval-implicit-progn-body ast))

  (:method ((ast lexical-application-form))
    ;; TODO: extend environment
    (bind ((*environment* (clone-environment))
           (lambda-ast (code-of ast)))
      (eval-implicit-progn-body lambda-ast))))
