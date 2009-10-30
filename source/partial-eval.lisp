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
   (bindings nil :documentation "A list of name value pairs, where name is a symbol and value is walked-form")))

(def (function e) make-environment (&key assumptions bindings)
  (make-instance 'environment
                 :assumptions assumptions
                 :bindings bindings))

(def function clone-environment ()
  (partial-eval.debug "Cloning environment")
  (make-instance 'environment
                 :assumptions (assumptions-of *environment*)
                 :bindings (bindings-of *environment*)))

;;;;;;
;;; Variable bindings

(def constant +unbound-variable+ '+unbound-variable+)

(def function variable-binding (name)
  (assert (symbolp name))
  (aprog1 (getf (bindings-of *environment*) name +unbound-variable+)
    (partial-eval.debug "Retrieving variable binding ~A results in ~A" name it)))

(def function (setf variable-binding) (new-value name)
  (partial-eval.debug "Changing variable binding ~A to ~A" name new-value)
  (assert (symbolp name))
  (setf (getf (bindings-of *environment*) name) new-value))

(def function extend-bindings (bindings)
  (dolist (binding bindings)
    (setf (variable-binding (car binding)) (cdr binding))))

;;;;;;
;;; Assumptions

(def function extend-assumptions (assumption)
  (partial-eval.debug "Extending assumptions with ~A" assumption)
  (push assumption (assumptions-of *environment*)))

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

(def (layered-function e) partial-eval-function-call (ast operator arguments)
  (:method ((ast free-application-form) operator arguments)
    (partial-eval.debug "Leaving function call to ~A intact" operator)
    (make-instance 'free-application-form
                   :operator operator
                   :arguments arguments)))

(def (layered-function e) lookup-variable-value? (name)
  (:method (name)
    #f))

(def (layered-function e) may-do-side-effect? (ast)
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
    (not (eval-function-call? ast (operator-of ast) (arguments-of ast)))))

(def (layered-function e) does-side-effect? (ast)
  (:method ((ast constant-form))
    nil))

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
             ;; FIXME this is most probably broken with special-variables, and their handling might even need some walker updates
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
                                        ,(list 'quote (mapcar '%partial-eval argument-values))
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
  ;; FIXME delme? seems to be dead code
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

(def (layered-function e) %partial-eval (form)
  (:documentation "This function is the recursive variant of PARTIAL-EVAL.")

  (:method ((ast constant-form))
    ast)

  (:method ((ast if-form))
    (bind ((evaluated-condition (%partial-eval (condition-of ast))))
      (flet ((partial-eval-then ()
               (bind ((*environment* (clone-environment))
                      (form (if (typep evaluated-condition 'constant-form)
                                (unwalk-form (condition-of ast))
                                (unwalk-form evaluated-condition))))
                 (if (and (consp form)
                          (member (first form) '(eq eql =)))
                     (extend-assumptions form)
                     (extend-assumptions `(not (eq #f ,form))))
                 (%partial-eval (then-of ast))))
             (partial-eval-else ()
               (bind ((*environment* (clone-environment))
                      (form (if (typep evaluated-condition 'constant-form)
                                (unwalk-form (condition-of ast))
                                (unwalk-form evaluated-condition))))
                 (extend-assumptions `(eq #f ,form))
                 (%partial-eval (else-of ast)))))
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
                            :body (list evaluated-body)))
            (t evaluated-body))))

  (:method ((ast return-from-form))
    (make-instance 'return-from-form
                   :target-block (target-block-of ast)
                   :result (%partial-eval (result-of ast))))

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
    (bind ((value (%partial-eval (value-of ast)))
           (variable (variable-of ast)))
      (if (typep variable 'free-variable-reference-form)
          (make-instance 'setq-form
                         :variable variable
                         :value value)
          (setf (variable-binding (name-of variable)) value))))

  (:method ((ast lexical-variable-binder-form))
    (bind ((let*-form? (typep ast 'let*-form))
           (*environment* (clone-environment))
           (bindings (mapcar (lambda (binding)
                               ;; FIXME binding is not a cons anymore, but a lexical-variable-binding-form
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
                                (not (may-do-side-effect? key))
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
      (if (eq +unbound-variable+ value)
          ast
          (if (may-do-side-effect? value)
              ast
              value))))

  (:method ((ast free-variable-reference-form))
    (bind ((value (variable-binding (name-of ast))))
      (if (eq +unbound-variable+ value)
          ast
          (if (may-do-side-effect? value)
              ast
              value))))

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
    (partial-eval.debug "Evaluating Lambda function form ~A" ast)
    (partial-eval-implicit-progn-body ast))

  (:method ((ast free-application-form))
    (bind ((operator (operator-of ast))
           (arguments (mapcar #'%partial-eval (arguments-of ast))))
      (cond ((and (eval-function-call? ast operator arguments)
                  (every (of-type '(or constant-form free-function-object-form)) arguments))
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
                       (bind ((*environment* (clone-environment))
                              (lambda-ast (walk-form source)))
                         (partial-eval.debug "Inlining function call to ~A ~A" operator source)
                         (partial-eval-lambda-list (arguments-of lambda-ast) arguments)
                         (partial-eval-implicit-progn-body lambda-ast))
                       (make-instance 'free-application-form
                                      :operator operator
                                      :arguments arguments))
                 (give-up nil ast))))
            (t (partial-eval-function-call ast operator arguments)))))

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
           (lambda-ast (definition-of ast))
           (argument-values (mapcar #'%partial-eval (arguments-of ast))))
      (partial-eval.debug "Lexical function application ~A for arguments ~A with values ~A"
                          (operator-of ast) (arguments-of lambda-ast) argument-values)
      (partial-eval-lambda-list (arguments-of lambda-ast) argument-values)
      (partial-eval-implicit-progn-body lambda-ast)))

  (:method ((ast the-form))
    (%partial-eval (value-of ast))))

(def (function e) partial-eval (form &key (environment (make-environment)) (layer 'standard-partial-eval-layer))
  "The function PARTIAL-EVAL takes a lisp FORM and returns another lisp FORM. The resulting form should,
in all possible circumstances, produce the same return value, the same side effects in the same order,
and the same non local exits, as the original FORM would have produced. The ENVIRONMENT parameter specifies
the initial assumptions in which the form should be evaluated. The LAYER parameter provides a way to customize
the standard partial evaluation logic to your needs."
  (bind ((*environment* environment))
    ;; KLUDGE: shall we really ignore?
    (with-active-layers (ignore-undefined-references)
      (funcall-with-layer-context
       (if layer
           (adjoin-layer layer (current-layer-context))
           (current-layer-context))
       (lambda ()
         (unwalk-form (%partial-eval (walk-form form))))))))
