;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval)

;;;;;;
;;; Function with source

(def (definer e) function-with-source (name args documentation &body forms)
  `(progn
     (setf (function-source ',name) `(lambda ,',args ,',@forms))
     (def function ,name ,args ,documentation ,@forms)))

(def special-variable *function-sources* (make-hash-table))

(def function function-source (name)
  (gethash name *function-sources*))

(def function (setf function-source) (form name)
  (setf (gethash name *function-sources*) form))

;;;;;;
;;; Environment

(def special-variable *environment*)

(def class* environment ()
  ((assumptions nil)
   (bindings nil)))

(def function make-empty-environment ()
  (make-instance 'environment))

(def function clone-environment ()
  (partial-eval.debug "Cloning environment")
  (make-instance 'environment
                 :assumptions (assumptions-of *environment*)
                 :bindings (bindings-of *environment*)))

(def function side-effect-free-function? (operator)
  (member operator '(eq car cdr cons consp eql + - * / 1+ 1- = < <= > >= length elt aref floor ceiling round)))

(def function variable-binding (name)
  (assert (symbolp name))
  (aprog1 (getf (bindings-of *environment*) name :unbound)
    (partial-eval.debug "Retrieving variable binding ~A results in ~A" name it)))

(def function (setf variable-binding) (new-value name)
  (partial-eval.debug "Changing variable binding ~A to ~A" name new-value)
  (assert (symbolp name))
  (setf (getf (bindings-of *environment*) name) new-value))

(def function extend-bindings (bindings)
  (dolist (binding bindings)
    (bind ((name (car binding))
           (value (cdr binding)))
      (setf (variable-binding name) value))))

(def function extend-assumptions (assumption)
  (partial-eval.debug "Extending assumptions ~A" assumption)
  (push assumption (assumptions-of *environment*)))

;;;;;;
;;; Utility

(def function make-progn-form (body)
  (cond ((null body)
         (make-instance 'constant-form :value nil))
        ((length= body 1)
         (first body))
        (t
         (make-instance 'progn-form :body body))))

(def generic collect-potential-side-effects (ast))

(def generic may-do-side-effect? (ast)
  (:method ((ast constant-form))
    #f)

  (:method ((ast if-form))
    (or (may-do-side-effect? (condition-of ast))
        (may-do-side-effect? (then-of ast))
        (may-do-side-effect? (else-of ast))))

  (:method ((ast implicit-progn-mixin))
    (dolist (body-ast (body-of ast))
      (when (may-do-side-effect? body-ast)
        (return #t))))

  (:method ((ast block-form))
    #f)

  (:method ((ast return-from-form))
    #f)

  (:method ((ast tagbody-form))
    #f)

  (:method ((ast go-tag-form))
    #f)

  (:method ((ast go-form))
    #f)

  (:method ((ast setq-form))
    #t)

  (:method ((ast variable-reference-form))
    #f)

  (:method ((ast free-application-form))
    (not (side-effect-free-function? (operator-of ast)))))

(def generic does-side-effect? (ast)
  (:method ((ast constant-form))
    nil))

(def generic collect-potential-non-local-exits (ast)
  (:method ((ast constant-form))
    nil)

  (:method ((ast if-form))
    (append (collect-potential-non-local-exits (condition-of ast))
            (collect-potential-non-local-exits (then-of ast))
            (collect-potential-non-local-exits (else-of ast))))

  (:method ((ast implicit-progn-mixin))
    (mappend #'collect-potential-non-local-exits (body-of ast)))

  (:method ((ast return-from-form))
    (list ast))

  (:method ((ast go-tag-form))
    nil)

  (:method ((ast go-form))
    (list ast))

  (:method ((ast variable-reference-form))
    nil)

  (:method ((ast free-application-form))
    nil))

(def generic may-do-non-local-exit? (ast)
  (:method ((ast constant-form))
    #f)

  (:method ((ast if-form))
    (or (may-do-non-local-exit? (condition-of ast))
        (may-do-non-local-exit? (then-of ast))
        (may-do-non-local-exit? (else-of ast))))

  (:method ((ast implicit-progn-mixin))
    (dolist (body-ast (body-of ast))
      (when (may-do-non-local-exit? body-ast)
        (return #t))))

  (:method ((ast return-from-form))
    #t)

  (:method ((ast go-tag-form))
    #f)

  (:method ((ast go-form))
    #t)

  (:method ((ast variable-reference-form))
    #f)

  (:method ((ast free-application-form))
    #t))

(def generic does-non-local-exit? (ast)
  (:method ((ast constant-form))
    #f)

  (:method ((ast implicit-progn-mixin))
    (dolist (body-ast (body-of ast))
      (awhen (does-non-local-exit? body-ast)
        (return it))))

  (:method ((ast if-form))
    (does-non-local-exit? (condition-of ast)))

  (:method ((ast return-from-form))
    ast)

  (:method ((ast go-tag-form))
    #f)

  (:method ((ast go-form))
    ast)

  (:method ((ast setq-form))
    #f)

  (:method ((ast variable-reference-form))
    #f)

  (:method ((ast free-application-form))
    #f))

(def function variable-referenced? (name ast)
  (map-ast (lambda (ast)
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

;;;;;;
;;; Partial eval

(def function partial-eval-bindings (bindings)
  (mapcar (lambda (binding)
            (bind ((name (car binding))
                   (value (partial-eval (cdr binding))))
              (cons name value)))
          bindings))

(def function partial-eval-implicit-progn-body (ast)
  (iter (for body-ast-cell :on (if (consp ast)
                                   ast
                                   (body-of ast)))
        (for body-ast = (car body-ast-cell))
        (for evaluated-ast = (partial-eval body-ast))
        (when evaluated-ast
          (when (or (null (cdr body-ast-cell))
                    (may-do-side-effect? evaluated-ast)
                    (may-do-non-local-exit? evaluated-ast))
            (collect evaluated-ast :into result))
          (when (does-non-local-exit? evaluated-ast)
            (return (make-progn-form result))))
        (finally (return (make-progn-form result)))))

(def (generic e) partial-eval (form)
  (:documentation "
PARTIAL-EVAL handles constants, function application and the following special forms:
  IF, PROGN, BLOCK, RETURN-FROM, TAGBODY, GO, LET, LET*, SETQ, FLET and LABELS.

The FORM parameter is either a lisp form or an instance of WALKED-FORM.

Partial evaluating a form results in a form that produces the same return value,
the same side effects and the same non local return.
")

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
      (bind ((evaluated-condition (partial-eval (condition-of ast))))
        (partial-eval.debug "Checking condition ~A" evaluated-condition)
        (if (typep evaluated-condition 'constant-form)
            (if (value-of evaluated-condition)
                (eval-then)
                (eval-else))
            (make-instance 'if-form
                           :condition evaluated-condition
                           :then (eval-then)
                           :else (eval-else))))))

  (:method ((ast progn-form))
    (partial-eval-implicit-progn-body ast))

  (:method ((ast block-form))
    (bind ((evaluated-body (partial-eval-implicit-progn-body ast))
           (non-local-exit (does-non-local-exit? evaluated-body))
           (all-non-local-exits (collect-potential-non-local-exits evaluated-body)))
      (cond ((and non-local-exit
                  (length= 1 all-non-local-exits)
                  (eq non-local-exit (first all-non-local-exits))
                  (typep non-local-exit 'return-from-form)
                  (eq ast (target-block-of non-local-exit)))
             (partial-eval (result-of non-local-exit)))
            ((block-referenced? ast evaluated-body)
             (make-instance 'block-form
                            :name (name-of ast)
                            :body (body-of evaluated-body)))
            (t evaluated-body))))

  (:method ((ast return-from-form))
    ast)

  (:method ((ast tagbody-form))
    (iter (with body = ast)
          (bind ((evaluated-body (partial-eval-implicit-progn-body body))
                 (non-local-exit (does-non-local-exit? evaluated-body)))
            (if (typep evaluated-body 'progn-form)
                (appending (body-of evaluated-body) :into result)
                (collect evaluated-body :into result))
            (when (and non-local-exit
                       (typep non-local-exit 'go-form)
                       (eq ast (enclosing-tagbody-of non-local-exit)))
              (setf body (jump-target-of non-local-exit))
              ;;(break "~A" (princ-to-string (unwalk-form (make-progn-form result))))
              (when body
                (next-iteration)))
            (return
              (aif non-local-exit
                   (make-progn-form (remove-if (of-type 'go-form) result))
                   (make-instance 'constant-form :value nil))))))

  (:method ((ast go-tag-form))
    ast)

  (:method ((ast go-form))
    ast)

  (:method ((ast setq-form))
    (bind ((value (partial-eval (value-of ast)))
           (variable (variable-of ast)))
      (if (typep variable 'free-variable-reference-form)
          (make-instance 'setq-form
                         :variable variable
                         :value value)
          (progn
            (setf (variable-binding (name-of variable)) value)
            nil))))

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
      (bind ((evaluated-body (partial-eval-implicit-progn-body ast))
             (runtime-bindings (remove-if-not (lambda (binding)
                                                (bind ((name (car binding)))
                                                  (variable-referenced? name evaluated-body)))
                                              bindings)))
        (if runtime-bindings
            (make-instance (class-of ast)
                           :bindings runtime-bindings
                           :body (list evaluated-body))
            evaluated-body))))

  (:method ((ast lexical-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if (eq :unbound value)
          ast
          (if (may-do-side-effect? value)
              ast
              value))))

  (:method ((ast free-variable-reference-form))
    ast)

  (:method ((ast lambda-function-form))
    (make-instance 'lambda-function-form
                   :arguments (arguments-of ast)
                   :body (list (partial-eval-implicit-progn-body ast))))

  (:method ((ast free-application-form))
    (bind ((operator (operator-of ast)))
      (if (side-effect-free-function? operator)
          (bind ((arguments (mapcar #'partial-eval (arguments-of ast))))
            ;; TODO: should check assumptions in *environment*, because we may already have the return value there
            ;;       or we can infer the return value from the assumptions
            (if (every (of-type 'constant-form) arguments)
                (make-instance 'constant-form
                               :value (apply operator (mapcar #'value-of arguments)))
                (make-instance 'free-application-form
                               :operator operator
                               :arguments arguments)))
          (bind ((source (function-source operator)))
            (if source
                (bind ((*environment* (clone-environment))
                       (lambda-ast (walk-form source)))
                  (extend-bindings (partial-eval-bindings (mapcar #'cons
                                                                  (mapcar #'name-of (arguments-of lambda-ast))
                                                                  (arguments-of ast))))
                  (partial-eval-implicit-progn-body lambda-ast))
                (make-instance 'free-application-form
                               :operator operator
                               :arguments (mapcar #'partial-eval (arguments-of ast))))))))

  (:method ((ast flet-form))
    (partial-eval-implicit-progn-body ast))

  (:method ((ast labels-form))
    (partial-eval-implicit-progn-body ast))

  (:method ((ast lexical-application-form))
    ;; TODO: extend environment
    (bind ((*environment* (clone-environment))
           (lambda-ast (code-of ast)))
      (partial-eval-implicit-progn-body lambda-ast))))
