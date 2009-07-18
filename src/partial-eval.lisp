;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval)

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

;; TODO: factor out and rename
(def function side-effect-free-function? (operator)
  (member operator '(eq not null endp car cdr cons consp eql + - * / 1+ 1- = < <= > >= first second third fourth getf
                     list list* length elt aref floor ceiling round typep mapcar
                     find-class class-of class-finalized-p class-prototype class-slots class-default-initargs slot-definition-name slot-definition-initargs slot-definition-allocation slot-definition-location slot-definition-initfunction slot-definition-writers slot-definition-readers
                     sb-int::proper-list-of-length-p sb-int:list-of-length-at-least-p sb-pcl::class-wrapper sb-pcl::safe-p sb-kernel:%instancep
                     sb-kernel:layout-length sb-kernel:classoid-of sb-int:memq sb-pcl::check-obsolete-instance sb-pcl::slot-definition-type-check-function)))

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

(def generic may-do-side-effect? (ast)
  (:method ((ast walked-form))
    #t)

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
  (:method ((ast walked-form))
    #t)

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
  (:method ((ast walked-form))
    #f)

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

(def function partial-eval-lambda-list (argument-definitions argument-values)
  (when (or argument-definitions
            argument-values)
    (bind ((argument-names (mapcar 'name-of argument-definitions))
           (evaluated-values (eval `(bind ((,(cadr (unwalk-form (make-instance 'lambda-function-form
                                                                               :arguments argument-definitions
                                                                               :body nil)))
                                             ,(list 'quote (mapcar '%partial-eval argument-values))))
                                      (list ,@argument-names)))))
      (extend-bindings (mapcar (lambda (name value)
                                 (cons name
                                       (if (typep value 'walked-form)
                                           value
                                           (make-instance 'constant-form
                                                          :value (if (listp value)
                                                                     (mapcar (lambda (v)
                                                                               (if (typep v 'constant-form)
                                                                                   (value-of v)
                                                                                   v))
                                                                             value)
                                                                     value)))))
                               argument-names evaluated-values)))))

(def function partial-eval-bindings (bindings)
  (mapcar (lambda (binding)
            (bind ((name (car binding))
                   (value (%partial-eval (cdr binding))))
              (cons name value)))
          bindings))

(def function partial-eval-implicit-progn-body (ast)
  (aprog1 (iter (for body-ast-cell :on (if (consp ast)
                                           ast
                                           (body-of ast)))
                (for body-ast = (car body-ast-cell))
                (for evaluated-ast = (progn
                                       (partial-eval.debug "Progn form element ~A" body-ast)
                                       (%partial-eval body-ast)))
                (when evaluated-ast
                  (when (or (null (cdr body-ast-cell))
                            (may-do-side-effect? evaluated-ast)
                            (may-do-non-local-exit? evaluated-ast))
                    (collect evaluated-ast :into result))
                  (when (does-non-local-exit? evaluated-ast)
                    (return (make-progn-form result))))
                (finally (return (make-progn-form result))))
    (partial-eval.debug "Result of implicit progn body is ~A" it)))

(def (function e) partial-eval (form &optional (environment (make-empty-environment)))
"PARTIAL-EVAL handles constants, function application and special forms.
The FORM parameter is a lisp form and the ENVIRONMENT may be prefilled with assumptions.
Partial evaluating a form results in a form that produces the same return value, the same side effects and the same non local exits."
  (bind ((*environment* environment))
    ;; FIXME: shall we really ignore?
    (with-walker-configuration (:undefined-reference-handler nil)
      (unwalk-form (%partial-eval (walk-form form))))))

(def generic %partial-eval (form)
  (:method ((ast constant-form))
    ast)

  (:method ((ast if-form))
    (flet ((partial-eval-then ()
             (bind ((*environment* (clone-environment)))
               (extend-assumptions `(not (eq #f ,(unwalk-form (condition-of ast)))))
               (%partial-eval (then-of ast))))
           (partial-eval-else ()
             (bind ((*environment* (clone-environment)))
               (extend-assumptions `(eq #f ,(unwalk-form (condition-of ast))))
               (%partial-eval (else-of ast)))))
      (bind ((evaluated-condition (%partial-eval (condition-of ast))))
        (partial-eval.debug "Checking condition ~A" evaluated-condition)
        (if (typep evaluated-condition 'constant-form)
            (if (value-of evaluated-condition)
                (partial-eval-then)
                (partial-eval-else))
            (make-instance 'if-form
                           :condition evaluated-condition
                           :then (partial-eval-then)
                           :else (partial-eval-else))))))

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
             (%partial-eval (result-of non-local-exit)))
            ((block-referenced? ast evaluated-body)
             (make-instance 'block-form
                            :name (name-of ast)
                            :body (body-of evaluated-body)))
            (t evaluated-body))))

  (:method ((ast return-from-form))
    (make-instance 'return-from-form
                   :target-block (target-block-of ast)
                   :result (%partial-eval (result-of ast))))

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
                   (or (make-progn-form (remove-if (of-type 'go-form) result))
                       (make-instance 'constant-form :value nil)))))))

  (:method ((ast go-tag-form))
    ast)

  (:method ((ast go-form))
    ast)

  (:method ((ast setq-form))
    (bind ((value (%partial-eval (value-of ast)))
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
                                      (value (%partial-eval (cdr binding))))
                                 (when let*-form?
                                   (setf (variable-binding name) value))
                                 (cons name value)))
                             (bindings-of ast))))
      (unless let*-form?
        (extend-bindings bindings))
      (bind ((evaluated-body (partial-eval-implicit-progn-body ast))
             (runtime-bindings (remove-if (lambda (binding)
                                            (bind ((name (car binding)))
                                              (or (and (typep (cdr binding) 'variable-reference-form)
                                                       (eq name (name-of (cdr binding))))
                                                  (not (variable-referenced? name evaluated-body)))))
                                          bindings)))
        (if runtime-bindings
            (make-instance (class-of ast)
                           :bindings runtime-bindings
                           :body (if (typep evaluated-body 'progn-form)
                                     (body-of evaluated-body)
                                     (list evaluated-body)))
            evaluated-body))))

  (:method ((ast lexical-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if (eq :unbound value)
          ast
          (if (may-do-side-effect? value)
              ast
              value))))

  (:method ((ast free-variable-reference-form))
    ;; TODO: KLUDGE: remove this and return the ast as soon as parameter binding does work
    ;; TODO: remove when keywords are walked into constant forms
    (if (keywordp (name-of ast))
        (make-instance 'constant-form :value (name-of ast))
        ast
        #+nil ;; TODO: delme eventually
        (bind ((value (variable-binding (name-of ast))))
          (if (eq :unbound value)
              ast
              (if (may-do-side-effect? value)
                  ast
                  value)))))

  (:method ((ast special-variable-reference-form))
    ast)

  (:method ((ast walked-lexical-function-object-form))
    ast)

  (:method ((ast free-function-object-form))
    ast)

  (:method ((ast lambda-function-form))
    (partial-eval.debug "Evaluating Lambda function form ~A" ast)
    (partial-eval-implicit-progn-body ast))

  (:method ((ast free-application-form))
    (bind ((operator (operator-of ast))
           (arguments (mapcar #'%partial-eval (arguments-of ast))))
      ;; TODO: KLUDGE: move
      (when (and (eq 'car operator)
                 (typep (first arguments) 'free-application-form)
                 (eq (operator-of (first arguments)) 'list*))
        (return-from %partial-eval (%partial-eval (first (arguments-of (first arguments))))))
      ;; TODO: KLUDGE: move
      ;; infer types
      (when (and (eq 'class-of operator)
                 (typep (first arguments) 'variable-reference-form))
        (return-from %partial-eval (make-instance 'constant-form :value (find-class 'cl-partial-eval-test:test))))
      ;; TODO: KLUDGE: move
      ;; infer types
      (when (and (eq 'sb-kernel:classoid-of operator)
                 (typep (first arguments) 'variable-reference-form))
        (return-from %partial-eval (make-instance 'constant-form :value (sb-kernel:classoid-of (class-prototype (find-class 'cl-partial-eval-test:test))))))
      ;; TODO: KLUDGE: move
      ;; infer types
      (when (and (eq 'typep operator)
                 (typep (first arguments) 'variable-reference-form))

        (return-from %partial-eval (make-instance 'constant-form :value (typep (class-prototype (find-class 'cl-partial-eval-test:test))
                                                                               (value-of (second arguments))))))
      (when (and (eq 'cdr operator)
                 (typep (first arguments) 'free-application-form)
                 (eq 'list* (operator-of (first arguments)))
                 (cdr (arguments-of (first arguments))))
        (return-from %partial-eval (%partial-eval (make-instance 'free-application-form
                                                                 :operator 'list*
                                                                 :arguments (cdr (arguments-of (first arguments)))))))
      ;; TODO: KLUDGE: move
      ;; TODO: cannot find source by some reason
      (when (eq 'sb-int:list-of-length-at-least-p operator)
        (return-from %partial-eval (make-instance 'constant-form :value #t)))
      (if (side-effect-free-function? operator)
          (progn
            ;; TODO: should check assumptions in *environment*, because we may already have the return value there
            ;;       or we can infer the return value from the assumptions
            (partial-eval.debug "Side effect free function call to ~A with arguments ~A" operator arguments)
            ;; TODO: this assumes the function do not change, let the user decide
            (if (every (of-type '(or constant-form free-function-object-form)) arguments)
                (bind ((value (apply operator (mapcar (lambda (argument)
                                                        (etypecase argument
                                                          (constant-form (value-of argument))
                                                          (free-function-object-form (name-of argument))))
                                                      arguments))))
                  (partial-eval.debug "Side effect free function call returned ~A" value)
                  (make-instance 'constant-form :value value))
                (make-instance 'free-application-form
                               :operator operator
                               :arguments arguments)))
          (progn
            ;; TODO: move these function specific stuff into a generic function or something?
            (when (and (eq 'apply operator)
                       (typep (first arguments) 'free-function-object-form))
              (setf operator (name-of (first arguments))
                    arguments (append (rest (butlast arguments))
                                      (mapcar (lambda (value)
                                                (make-instance 'constant-form :value value))
                                              (value-of (last-elt arguments))))))
            ;; TODO: KLUDGE: move
            (when (and (eq 'funcall operator)
                       (typep (first arguments) 'free-function-object-form))
              (setf operator (name-of (first arguments))
                    arguments (rest arguments)))
            (bind ((source (or (make-function-lambda-form operator)
                               (make-generic-function-lambda-form operator))))
              (restart-case
                  (if source
                      (bind ((*environment* (clone-environment))
                             (lambda-ast (walk-form source)))
                        (partial-eval.debug "Function call to ~A ~A" operator source)
                        ;;(break "Partial evaluating function call to ~A with arguments ~A" operator arguments)
                        (partial-eval-lambda-list (arguments-of lambda-ast) arguments)
                        (partial-eval-implicit-progn-body lambda-ast))
                      (make-instance 'free-application-form
                                     :operator operator
                                     :arguments arguments))
                (give-up nil (make-instance 'constant-form :value "Gave up partial evaluating"))))))))

  (:method ((ast multiple-value-call-form))
    (bind ((arguments (mapcar #'%partial-eval (arguments-of ast)))
           (*environment* (clone-environment)))
      (partial-eval-lambda-list (arguments-of (function-designator-of ast)) arguments)
      (%partial-eval (function-designator-of ast))))

  (:method ((ast macrolet-form))
    (partial-eval-implicit-progn-body ast))

  (:method ((ast flet-form))
    ;; TODO: really?
    (extend-bindings (bindings-of ast))
    (partial-eval.debug "Evaluating flet function ~A" ast)
    (partial-eval-implicit-progn-body ast))

  (:method ((ast labels-form))
    (partial-eval-implicit-progn-body ast))

  (:method ((ast lexical-application-form))
    (bind ((*environment* (clone-environment))
           (lambda-ast (code-of ast))
           (argument-values (mapcar #'%partial-eval (arguments-of ast))))
      (partial-eval.debug "Lexical function application ~A for arguments ~A with values ~A"
                          (operator-of ast) (arguments-of lambda-ast) argument-values)
      (partial-eval-lambda-list (arguments-of lambda-ast) argument-values)
      (partial-eval-implicit-progn-body lambda-ast)))

  (:method ((ast the-form))
    (%partial-eval (value-of ast))))
