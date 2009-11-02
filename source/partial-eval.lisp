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

(def (function e) make-partial-eval-environment (&key assumptions bindings types)
  (make-instance 'environment
                 :assumptions assumptions
                 :bindings bindings
                 :types types))

(def (function e) clone-partial-eval-environment ()
  (partial-eval.debug "Cloning environment")
  (make-instance 'environment
                 :assumptions (copy-seq (assumptions-of *environment*))
                 :bindings (copy-seq (bindings-of *environment*))
                 :types (copy-seq (types-of *environment*))))

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
    (setf (variable-binding (car binding)) (cdr binding))))

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

;;;;;;
;;; Customization points

(def (layered-function e) eval-function-call? (ast operator arguments)
  (:documentation "Returns TRUE if the function call should be evaluated at partial eval time, FALSE otherwise.")

  (:method ((ast free-application-form) operator arguments)
    #f))

(def (layered-function e) inline-function-call? (ast operator arguments)
  (:documentation "Returns TRUE if the function call should be inlined at partial eval time, FALSE otherwise.")

  (:method ((ast free-application-form) operator arguments)
    #f))

(def (layered-function e) lookup-variable-value? (name)
  (:method (name)
    #f))

(def (layered-function e) may-have-side-effect? (ast)
  (:method ((ast walked-form))
    #t)

  (:method ((ast constant-form))
    #f)

  (:method ((ast if-form))
    (or (may-have-side-effect? (condition-of ast))
        (may-have-side-effect? (then-of ast))
        (may-have-side-effect? (else-of ast))))

  (:method ((ast implicit-progn-mixin))
    (dolist (body-ast (body-of ast))
      (when (may-have-side-effect? body-ast)
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
    (not (eval-function-call? ast (operator-of ast) (arguments-of ast)))))

(def (layered-function e) collect-potential-non-local-exits (ast)
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

  (:method ((ast the-form))
    (collect-potential-non-local-exits (value-of ast)))

  (:method ((ast variable-reference-form))
    nil)

  (:method ((ast free-application-form))
    nil)

  (:method ((ast setq-form))
    nil)

  (:method ((ast walked-lexical-application-form))
    (collect-potential-non-local-exits (definition-of ast))))

(def (layered-function e) may-do-non-local-exit? (ast)
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
    (not (eval-function-call? ast (operator-of ast) (arguments-of ast)))))

(def (layered-function e) does-non-local-exit? (ast)
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

;;;;;;
;;; Util

(def function make-progn-form (body)
  (cond ((null body)
         (make-instance 'constant-form :value nil))
        ((length= body 1)
         (first body))
        (t
         (make-instance 'progn-form :body body))))

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

;;;;;;
;;; Partial eval

(def function partial-eval-lambda-list (argument-definitions argument-values)
  (when (or argument-definitions
            argument-values)
    (bind ((argument-names (mappend (lambda (argument-definition)
                                      (typecase argument-definition
                                        (optional-function-argument-form (list (name-of argument-definition)
                                                                               (supplied-p-parameter-name-of argument-definition)))
                                        (function-argument-form (list (name-of argument-definition)))))
                                    argument-definitions))
           ;; TODO: remove this eval and use alexandria
           (evaluated-values (eval `(destructuring-bind
                                          ,@(cdadr (unwalk-form (make-instance 'lambda-function-form
                                                                               :arguments argument-definitions
                                                                               :body nil)))
                                        ,(list 'quote (mapcar 'partial-eval-form argument-values))
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

(def function partial-eval-implicit-progn-body (ast)
  (aprog1 (iter (for body-ast-cell :on (if (consp ast)
                                           ast
                                           (body-of ast)))
                (for body-ast = (car body-ast-cell))
                (for evaluated-ast = (progn
                                       (partial-eval.debug "Progn form element ~A" body-ast)
                                       (partial-eval-form body-ast)))
                (when evaluated-ast
                  (when (or (null (cdr body-ast-cell))
                            (may-have-side-effect? evaluated-ast)
                            (may-do-non-local-exit? evaluated-ast))
                    (if (typep evaluated-ast 'progn-form)
                        (appending (body-of evaluated-ast) :into result)
                        (collect evaluated-ast :into result)))
                  (when (does-non-local-exit? evaluated-ast)
                    (return (make-progn-form result))))
                (finally (return (make-progn-form result))))
    (partial-eval.debug "Result of implicit progn body is ~A" it)))

(def (layered-function e) partial-eval-function-call (ast operator arguments)
  (:method ((ast free-application-form) operator arguments)
    (partial-eval.debug "Leaving function call to ~A intact" operator)
    (make-instance 'free-application-form
                   :operator operator
                   :arguments arguments)))

(def (layered-function e) partial-eval-form (form)
  (:documentation "This function is the recursive variant of PARTIAL-EVAL.")

  (:method ((ast constant-form))
    ast)

  (:method ((ast if-form))
    (bind ((evaluated-condition (partial-eval-form (condition-of ast))))
      (flet ((partial-eval-then ()
               (bind ((*environment* (clone-partial-eval-environment))
                      (form (if (typep evaluated-condition 'constant-form)
                                (unwalk-form (condition-of ast))
                                (unwalk-form evaluated-condition))))
                 (if (and (consp form)
                          (member (first form) '(eq eql =)))
                     (extend-assumptions form)
                     (extend-assumptions `(not (eq #f ,form))))
                 (partial-eval-form (then-of ast))))
             (partial-eval-else ()
               (bind ((*environment* (clone-partial-eval-environment))
                      (form (if (typep evaluated-condition 'constant-form)
                                (unwalk-form (condition-of ast))
                                (unwalk-form evaluated-condition))))
                 (extend-assumptions `(eq #f ,form))
                 (partial-eval-form (else-of ast)))))
        (partial-eval.debug "If condition evaluated to ~A" evaluated-condition)
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

  (:method ((ast multiple-value-prog1-form))
    ;; KLUDGE: this does not work properly when there are side effects or non local exits in the first form
    ;; TODO: does not return multiple-value
    (bind ((result (partial-eval-form (first-form-of ast))))
      ;; FIXME: this is utterly broken, bah
      (partial-eval-form (make-instance 'progn-form :body (list result
                                                            (partial-eval-implicit-progn-body (other-forms-of ast)))))))

  (:method ((ast block-form))
    (bind ((evaluated-body (partial-eval-implicit-progn-body ast))
           (non-local-exit (does-non-local-exit? evaluated-body))
           (all-non-local-exits (collect-potential-non-local-exits evaluated-body)))
      (cond ((and non-local-exit
                  (length= 1 all-non-local-exits)
                  (eq non-local-exit (first all-non-local-exits))
                  (typep non-local-exit 'return-from-form)
                  (eq ast (target-block-of non-local-exit)))
             (partial-eval-form (result-of non-local-exit)))
            ((block-referenced? ast evaluated-body)
             (make-instance 'block-form
                            :name (name-of ast)
                            :body (list evaluated-body)))
            (t evaluated-body))))

  (:method ((ast return-from-form))
    (make-instance 'return-from-form
                   :target-block (target-block-of ast)
                   :result (partial-eval-form (result-of ast))))

  (:method ((ast tagbody-form))
    (iter (with body = ast)
          (for count :from 0)
          (when (= count 10)
            (partial-eval.debug "Too many go statements evaluated, giving up unrolling ~A" ast)
            (return ast))
          (bind ((evaluated-body (partial-eval-implicit-progn-body body))
                 (non-local-exits (collect-potential-non-local-exits evaluated-body)))
            (if (typep evaluated-body 'progn-form)
                (appending (body-of evaluated-body) :into result)
                (collect evaluated-body :into result))
            (when (and (length= 1 non-local-exits)
                       (typep (first non-local-exits) 'go-form)
                       (eq ast (enclosing-tagbody-of (first non-local-exits))))
              (setf body (jump-target-of (first non-local-exits)))
              (when body
                (next-iteration)))
            (return
              (if non-local-exits
                  (if (length= 1 non-local-exits)
                      (progn
                        (partial-eval.debug "Unrolling ~A is finished successfully")
                        (make-progn-form (remove-if (of-type 'go-form) result)))
                      (progn
                        (partial-eval.debug "Too many non-local exists, giving up unrolling ~A" ast)
                        ast))
                  (progn
                    (partial-eval.debug "Unrolling ~A resulted in complete elimination")
                    (make-instance 'constant-form :value nil)))))))

  (:method ((ast go-tag-form))
    ast)

  (:method ((ast go-form))
    ast)

  (:method ((ast setq-form))
    (bind ((value (partial-eval-form (value-of ast)))
           (variable (variable-of ast)))
      (if (typep variable 'free-variable-reference-form)
          (make-instance 'setq-form
                         :variable variable
                         :value value)
          (setf (variable-binding (name-of variable)) value))))

  (:method ((ast lexical-variable-binder-form))
    (bind ((let*-form? (typep ast 'let*-form))
           (*environment* (clone-partial-eval-environment))
           (bindings (mapcar (lambda (binding)
                               ;; FIXME: binding is not a cons anymore, but a lexical-variable-binding-form
                               (bind ((name (car binding))
                                      (value (partial-eval-form (cdr binding))))
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
        ;; make common subexpressions local variables
        (bind ((body (make-instance 'progn-form
                                    :body (if (typep evaluated-body 'progn-form)
                                              (body-of evaluated-body)
                                              (list evaluated-body))))
               (seen-forms (make-hash-table)))
          (map-ast (lambda (ast)
                     (when (typep ast 'walked-form)
                       (if (gethash ast seen-forms)
                           (incf (gethash ast seen-forms))
                           (setf (gethash ast seen-forms) 1)))
                     ast)
                   body)
          (map nil 'funcall
               (iter (for (key value) :in-hashtable seen-forms)
                     (when (and (not (typep key '(or constant-form variable-reference-form)))
                                (not (may-have-side-effect? key))
                                (not (may-do-non-local-exit? key))
                                (> value 1))
                       (bind ((name (gensym)))
                         (push (cons name (bind ((class (class-of key)))
                                            (prog1-bind clone (make-instance class)
                                              (dolist (slot (class-slots class))
                                                (when (slot-boundp-using-class class key slot)
                                                  (setf (slot-value-using-class class clone slot) (slot-value-using-class class key slot)))))))
                               runtime-bindings)
                         (map-ast (lambda (ast)
                                    (when (eq key ast)
                                      (collect (bind ((key key))
                                                 (lambda ()
                                                   (change-class key 'lexical-variable-reference-form :name name)))))
                                    ast)
                                  body)))))
          (if runtime-bindings
              (make-instance 'let*-form #+nil (class-of ast) ;; TODO: based on whether if runtime-bindings were extended
                             :bindings runtime-bindings
                             :body (body-of body))
              evaluated-body)))))

  (:method ((ast lexical-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if value
          (if (may-have-side-effect? value)
              ast
              value)
          ast)))

  (:method ((ast free-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if value
          (if (may-have-side-effect? value)
              ast
              value)
          ast)))

  (:method ((ast special-variable-reference-form))
    (bind ((name (name-of ast)))
      (if (lookup-variable-value? name)
          (make-instance 'constant-form :value (symbol-value name))
          ast)))

  (:method ((ast walked-lexical-function-object-form))
    ast)

  (:method ((ast free-function-object-form))
    ast)

  (:method ((ast lambda-function-form))
    ast)

  (:method ((ast free-application-form))
    (bind ((operator (operator-of ast))
           (arguments (mapcar #'partial-eval-form (arguments-of ast))))
      (cond ((and (every (of-type '(or constant-form free-function-object-form)) arguments)
                  (eval-function-call? ast operator arguments))
             ;; TODO: should check assumptions in *environment*, because we may already have the return value there
             ;;       or we can infer the return value from the assumptions
             (partial-eval.debug "Immediately evaluating function call to ~A with constant arguments ~A" operator arguments)
             ;; TODO: this assumes the function does not change, let the user decide
             (prog1-bind value
                 (make-instance 'constant-form
                                :value (apply operator (mapcar (lambda (argument)
                                                                 (etypecase argument
                                                                   (constant-form (value-of argument))
                                                                   (free-function-object-form (name-of argument))))
                                                               arguments)))
               (partial-eval.debug "Evaluating function call returned ~A" value)))
            ((inline-function-call? ast operator arguments)
             (bind ((source (make-function-lambda-form operator)))
               (restart-case
                   (if source
                       (bind ((*environment* (clone-partial-eval-environment))
                              (lambda-ast (walk-form source)))
                         (bind ((*print-level* 3))
                           (partial-eval.debug "Inlining function call to ~A as ~A" operator source))
                         (partial-eval-lambda-list (arguments-of lambda-ast) arguments)
                         (partial-eval-implicit-progn-body lambda-ast))
                       (make-instance 'free-application-form
                                      :operator operator
                                      :arguments arguments))
                 (give-up nil ast))))
            (t
             (partial-eval.debug "Partial evaluating function call to ~A with arguments ~A" operator arguments)
             (partial-eval-function-call ast operator arguments)))))

  (:method ((ast multiple-value-call-form))
    (bind ((arguments (mapcar #'partial-eval-form (arguments-of ast)))
           (*environment* (clone-partial-eval-environment)))
      (partial-eval-lambda-list (arguments-of (function-designator-of ast)) arguments)
      (partial-eval-form (function-designator-of ast))))

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
    (bind ((*environment* (clone-partial-eval-environment))
           (lambda-ast (definition-of ast))
           (argument-values (mapcar #'partial-eval-form (arguments-of ast))))
      (partial-eval.debug "Lexical function application ~A for arguments ~A with values ~A"
                          (operator-of ast) (arguments-of lambda-ast) argument-values)
      (partial-eval-lambda-list (arguments-of lambda-ast) argument-values)
      (partial-eval-implicit-progn-body lambda-ast)))

  (:method ((ast the-form))
    (partial-eval-form (value-of ast))))

(def (function e) partial-eval (form &key (environment (make-partial-eval-environment)) (layer 'standard-partial-eval-layer))
  "The function PARTIAL-EVAL takes a lisp FORM and returns another lisp FORM. The resulting form should,
in all possible environments, produce the same return value, the same side effects in the same order,
and the same non local exits (in and out), as the original FORM would have produced. The ENVIRONMENT parameter
specifies the initial assumptions in which the form should be partially evaluated. The LAYER parameter provides 
a way to customize the standard partial evaluation logic to your needs."
  (bind ((*environment* environment))
    (with-active-layers (ignore-undefined-references)
      (funcall-with-layer-context (if layer
                                      (adjoin-layer layer (current-layer-context))
                                      (current-layer-context))
                                  (lambda ()
                                    (unwalk-form (partial-eval-form (walk-form form))))))))
