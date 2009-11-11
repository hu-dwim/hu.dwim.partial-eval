;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; partial-eval-layer

(def (layer* e) partial-eval-layer ()
  ((eval-function-calls)
   (lookup-variable-values)
   (inline-function-calls)))

(def constant +default-function-call-inline-limit+ 10)

(def special-variable *function-call-inline-level*)

(def constant +default-tagbody-go-unroll-limit+ 10)

;;;;;;
;;; may-type

(def type may-type ()
  '(member :never :sometimes :always))

(def function never? (value)
  (eq value :never))

(def function sometimes? (value)
  (eq value :sometimes))

(def function always? (value)
  (eq value :always))

(def function may-not (value)
  (ecase value
    (:never :always)
    (:sometimes :sometimes)
    (:always :never)))

(def function may-or (&rest values)
  (may-or* values))

(def function may-or* (values &key (key #'identity))
  (cond ((find :always values :key key)
         :always)
        ((find :sometimes values :key key)
         :sometimes)
        (t :never)))

(def function may-and (&rest values)
  (may-and* values))

(def function may-and* (values &key (key #'identity))
  (may-not (may-or* values :key key)))

;;;;;;
;;; eval-function-call?

(def layered-method eval-function-call? ((ast free-application-form) operator arguments)
  #f)

(def layered-method eval-function-call? :in partial-eval-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator (eval-function-calls-of (current-layer-prototype)) :test #'equal)))

;;;;;;
;;; inline-function-call?

(def layered-method inline-function-call? ((ast free-application-form) operator arguments)
  #f)

(def layered-method inline-function-call? :in partial-eval-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator (inline-function-calls-of (current-layer-prototype)) :test #'equal)))

;;;;;;
;;; lookup-variable-value?

(def layered-method lookup-variable-value? ((ast variable-reference-form) name)
  #f)

(def layered-method lookup-variable-value? :in partial-eval-layer ((ast variable-reference-form) name)
  (or (call-next-layered-method)
      (member name (lookup-variable-values-of (current-layer-prototype)))))

;;;;;;
;;; return-type

(def layered-method return-type ((ast walked-form))
  t)

(def layered-method return-type ((ast constant-form))
  (type-of (value-of ast)))

(def layered-method return-type ((ast variable-reference-form))
  (variable-type (name-of ast)))

;;;;;;
;;; returns-new-allocation?

(def layered-method returns-new-allocation? ((ast walked-form))
  :sometimes)

(def layered-method returns-new-allocation? ((ast constant-form))
  :never)

;;;;;;
;;; returns-locally?

(def layered-method returns-locally? ((ast walked-form))
  :sometimes)

(def layered-method returns-locally? ((ast constant-form))
  :always)

(def layered-method returns-locally? ((ast return-from-form))
  :never)

(def layered-method returns-locally? ((ast go-form))
  :never)

;; TODO: wrong direct subclass ordering (walked-form implicit-progn-mixin)
(def layered-method returns-locally? :around ((ast implicit-progn-mixin))
  (may-not (may-or* (body-of ast) :key 'exits-non-locally?)))

;;;;;;
;;; exits-non-locally?

(def layered-method exits-non-locally? ((ast walked-form))
  :sometimes)

(def layered-method exits-non-locally? ((ast constant-form))
  :never)

(def layered-method exits-non-locally? ((ast if-form))
  (may-or (exits-non-locally? (condition-of ast))
          (may-and (exits-non-locally? (then-of ast))
                   (exits-non-locally? (else-of ast)))))

;; TODO: wrong direct subclass ordering (walked-form implicit-progn-mixin)
(def layered-method exits-non-locally? :around ((ast implicit-progn-mixin))
  (may-or* (body-of ast) :key 'exits-non-locally?))

(def layered-method exits-non-locally? ((ast return-from-form))
  :always)

(def layered-method exits-non-locally? ((ast go-tag-form))
  :never)

(def layered-method exits-non-locally? ((ast go-form))
  :always)

(def layered-method exits-non-locally? ((ast the-form))
  (exits-non-locally? (value-of ast)))

(def layered-method exits-non-locally? ((ast variable-reference-form))
  :never)

(def layered-method exits-non-locally? ((ast lexical-variable-binding-form))
  (exits-non-locally? (initial-value-of ast)))

(def layered-method exits-non-locally? ((ast walked-lexical-application-form))
  (exits-non-locally? (definition-of ast)))

(def layered-method exits-non-locally? ((ast function-object-form))
  :never)

(def layered-method exits-non-locally? ((ast free-application-form))
  (if (eval-function-call? ast (operator-of ast) (arguments-of ast))
      :never
      :sometimes))

;;;;;;
;;; collect-non-local-exits

(def layered-method collect-non-local-exits ((ast walked-form))
  nil)

(def layered-method collect-non-local-exits ((ast constant-form))
  nil)

(def layered-method collect-non-local-exits ((ast if-form))
  (append (collect-non-local-exits (condition-of ast))
          (collect-non-local-exits (then-of ast))
          (collect-non-local-exits (else-of ast))))

;; TODO: wrong direct subclass ordering (walked-form implicit-progn-mixin)
(def layered-method collect-non-local-exits :around ((ast implicit-progn-mixin))
  (mappend #'collect-non-local-exits (body-of ast)))

(def layered-method collect-non-local-exits ((ast return-from-form))
  (list ast))

(def layered-method collect-non-local-exits ((ast go-tag-form))
  nil)

(def layered-method collect-non-local-exits ((ast go-form))
  (list ast))

(def layered-method collect-non-local-exits ((ast the-form))
  (collect-non-local-exits (value-of ast)))

(def layered-method collect-non-local-exits ((ast variable-reference-form))
  nil)

(def layered-method collect-non-local-exits ((ast free-application-form))
  nil)

(def layered-method collect-non-local-exits ((ast setq-form))
  nil)

(def layered-method collect-non-local-exits ((ast walked-lexical-application-form))
  (collect-non-local-exits (definition-of ast)))

;;;;;;
;;; has-side-effect?

(def layered-method has-side-effect? ((ast walked-form))
  :sometimes)

(def layered-method has-side-effect? ((ast constant-form))
  :never)

(def layered-method has-side-effect? ((ast if-form))
  (may-or (has-side-effect? (condition-of ast))
          (may-and (has-side-effect? (then-of ast))
                   (has-side-effect? (else-of ast)))))

;; TODO: wrong direct subclass ordering (walked-form implicit-progn-mixin)
(def layered-method has-side-effect? :around ((ast implicit-progn-mixin))
  (may-or* (body-of ast) :key 'has-side-effect?))

(def layered-method has-side-effect? ((ast return-from-form))
  :never)

(def layered-method has-side-effect? ((ast go-tag-form))
  :never)

(def layered-method has-side-effect? ((ast go-form))
  :never)

(def layered-method has-side-effect? ((ast setq-form))
  :always)

(def layered-method has-side-effect? ((ast the-form))
  (has-side-effect? (value-of ast)))

(def layered-method has-side-effect? ((ast variable-reference-form))
  :never)

(def layered-method has-side-effect? ((ast lexical-variable-binding-form))
  (has-side-effect? (initial-value-of ast)))

(def layered-method has-side-effect? ((ast function-object-form))
  :never)

(def layered-method has-side-effect? ((ast walked-lexical-application-form))
  (has-side-effect? (definition-of ast)))

(def layered-method has-side-effect? ((ast free-application-form))
  (if (eval-function-call? ast (operator-of ast) (arguments-of ast))
      :never
      :sometimes))

;;;;;;
;;; partial-eval-lambda-list

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
                                 (make-instance 'lexical-variable-binding-form
                                                :name name
                                                :initial-value (if (typep value 'walked-form)
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

;;;;;;
;;; partial-eval-implicit-progn

(def layered-method partial-eval-implicit-progn ((ast implicit-progn-mixin))
  (partial-eval-implicit-progn (body-of ast)))

(def layered-method partial-eval-implicit-progn ((ast list))
  (aprog1 (iter (for body-ast-cell :on ast)
                (for body-ast = (car body-ast-cell))
                (for last-form? = (null (cdr body-ast-cell)))
                (for evaluated-ast = (progn
                                       (partial-eval.debug "Progn form element ~A" body-ast)
                                       (partial-eval-form body-ast)))
                (for exists-non-locally? = (exits-non-locally? evaluated-ast))
                (when evaluated-ast
                  (when (or last-form?
                            (not (never? (has-side-effect? evaluated-ast)))
                            (not (never? exists-non-locally?)))
                    (etypecase evaluated-ast
                      (progn-form
                       (appending (body-of evaluated-ast) :into result))
                      (multiple-value-prog1-form
                       (if last-form?
                           (collect evaluated-ast :into result)
                           (progn
                             (collect (first-form-of evaluated-ast) :into result)
                             (appending (other-forms-of evaluated-ast) :into result))))
                      (t (collect evaluated-ast :into result))))
                  (when (always? exists-non-locally?)
                    (return (make-progn-form result))))
                (finally (return (make-progn-form result))))
    (partial-eval.debug "Result of implicit progn body is ~A" it)))

;;;;;;
;;; partial-eval-function-call

(def layered-method partial-eval-function-call ((ast free-application-form) operator arguments)
  (partial-eval.debug "Leaving function call to ~A intact" operator)
  (make-free-application-form operator arguments))

;;;;;;
;;; partial-eval-form

(def layered-method partial-eval-form ((ast constant-form))
  ast)

(def layered-method partial-eval-form ((ast if-form))
  (bind ((evaluated-condition (partial-eval-form (condition-of ast))))
    (flet ((partial-eval-then ()
             (bind ((form (if (typep evaluated-condition 'constant-form)
                              (unwalk-form (condition-of ast))
                              (unwalk-form evaluated-condition))))
               (if (and (consp form)
                        (member (first form) '(eq eql =)))
                   (extend-assumptions form)
                   (extend-assumptions `(not (eq #f ,form))))
               (partial-eval-form (then-of ast))))
           (partial-eval-else ()
             (bind ((form (if (typep evaluated-condition 'constant-form)
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
                         :then (bind ((*environment* (clone-environment)))
                                 (partial-eval-then))
                         :else (bind ((*environment* (clone-environment)))
                                 (partial-eval-else)))))))

(def layered-method partial-eval-form ((ast progn-form))
  (partial-eval-implicit-progn ast))

(def layered-method partial-eval-form ((ast multiple-value-prog1-form))
  (bind ((evaluated-first-form (partial-eval-form (first-form-of ast)))
         (evaluated-other-forms (partial-eval-implicit-progn (other-forms-of ast))))
    (if (and (never? (has-side-effect? evaluated-first-form))
             (never? (exits-non-locally? evaluated-first-form)))
        (if (typep evaluated-other-forms 'constant-form)
            evaluated-first-form
            (make-progn-form (list evaluated-other-forms
                                   evaluated-first-form)))
        (make-instance 'multiple-value-prog1-form
                       :first-form evaluated-first-form
                       :other-forms (list evaluated-other-forms)))))

(def layered-method partial-eval-form ((ast block-form))
  (bind ((evaluated-body (partial-eval-implicit-progn ast))
         (non-local-exits (collect-non-local-exits evaluated-body)))
    (cond ((and (always? (exits-non-locally? evaluated-body))
                (length= 1 non-local-exits)
                (typep (first non-local-exits) 'return-from-form)
                (eq ast (target-block-of (first non-local-exits))))
           (bind ((non-local-exit (first non-local-exits)))
             (aprog1 (if (not (never? (has-side-effect? evaluated-body)))
                         (make-instance 'progn-form :body (append (remove-if (of-type 'return-from-form) (body-of evaluated-body))
                                                                  (list (partial-eval-form (result-of non-local-exit)))))
                         (partial-eval-form (result-of non-local-exit)))
               (partial-eval.debug "Eliminated ~A by ~A, result is ~A" ast (first non-local-exits) it))))
          ((block-referenced? ast evaluated-body)
           (make-instance 'block-form
                          :name (name-of ast)
                          :body (list evaluated-body)))
          (t evaluated-body))))

(def layered-method partial-eval-form ((ast return-from-form))
  (make-instance 'return-from-form
                 :target-block (target-block-of ast)
                 :result (partial-eval-form (result-of ast))))

(def layered-method partial-eval-form ((ast tagbody-form))
  (iter (with body = ast)
        (for go-count :from 0)
        (when (= go-count +default-tagbody-go-unroll-limit+)
          (partial-eval.debug "Too many go statements evaluated, giving up unrolling ~A" ast)
          (return ast))
        (for evaluated-body = (partial-eval-implicit-progn body))
        (for non-local-exits = (collect-non-local-exits evaluated-body))
        (for gos = (remove-if-not (lambda (non-local-exit)
                                    (and (typep non-local-exit 'go-form)
                                         (eq ast (enclosing-tagbody-of non-local-exit))))
                                  non-local-exits))
        (for always-go? = (and (always? (exits-non-locally? evaluated-body))
                               (length= 1 gos)))
        (if (typep evaluated-body 'progn-form)
            (appending (body-of evaluated-body) :into result)
            (collect evaluated-body :into result))
        (setf body (when always-go?
                     (jump-target-of (first gos))))
        (unless body
          (return
            (if (or always-go?
                    (null gos))
                (bind ((result (remove-if (of-type '(or go-form go-tag-form)) result))
                       (result-ast (make-progn-form result)))
                  (aprog1 (if (or (never? (returns-locally? result-ast))
                                  (eq (return-type result-ast) 'null))
                              result-ast
                              (make-progn-form (append result (list (make-instance 'constant-form :value nil)))))
                    (partial-eval.debug "Eliminated ~A by unrolling, result is ~A" ast it)))
                (progn
                  (partial-eval.debug "Undecidable go statementes, giving up unrolling ~A" ast)
                  ast))))))

(def layered-method partial-eval-form ((ast go-tag-form))
  ast)

(def layered-method partial-eval-form ((ast go-form))
  ast)

(def layered-method partial-eval-form ((ast setq-form))
  (bind ((value (partial-eval-form (value-of ast)))
         (variable (variable-of ast)))
    (if (typep variable 'free-variable-reference-form)
        (make-instance 'setq-form
                       :variable variable
                       :value value)
        (setf (variable-binding (name-of variable)) value))))

(def layered-method partial-eval-form ((ast lexical-variable-binder-form))
  (bind ((let*-form? (typep ast 'let*-form))
         (bindings (mapcar (lambda (binding)
                             (bind ((name (name-of binding))
                                    (value (partial-eval-form (initial-value-of binding))))
                               (when let*-form?
                                 (setf (variable-binding name) value))
                               (make-instance 'lexical-variable-binding-form
                                              :name name
                                              :initial-value value)))
                           (bindings-of ast))))
    (unless let*-form?
      (extend-bindings bindings))
    (bind ((evaluated-body (partial-eval-implicit-progn ast))
           (runtime-bindings (remove-if (lambda (binding)
                                          (bind ((name (name-of binding)))
                                            (or (and (typep (initial-value-of binding) 'variable-reference-form)
                                                     (eq name (name-of (initial-value-of binding))))
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
                              (never? (has-side-effect? key))
                              (never? (exits-non-locally? key))
                              (> value 1))
                     (bind ((name (gensym)))
                       (push (make-instance 'lexical-variable-binding-form
                                            :name name
                                            :initial-value (bind ((class (class-of key)))
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

(def layered-method partial-eval-form ((ast variable-reference-form))
  (bind ((value (variable-binding (name-of ast))))
    (if (and value
             (never? (has-side-effect? value))
             (never? (exits-non-locally? value)))
        value
        ast)))

(def layered-method partial-eval-form ((ast special-variable-reference-form))
  (bind ((name (name-of ast)))
    (if (lookup-variable-value? ast name)
        (make-instance 'constant-form :value (symbol-value name))
        ast)))

(def layered-method partial-eval-form ((ast walked-lexical-function-object-form))
  ast)

(def layered-method partial-eval-form ((ast free-function-object-form))
  ast)

(def layered-method partial-eval-form ((ast lambda-function-form))
  ast)

(def layered-method partial-eval-form ((ast free-application-form))
  (bind ((operator (operator-of ast))
         (arguments (mapcar #'partial-eval-form (arguments-of ast))))
    (cond ((and (constant-values? arguments)
                (eval-function-call? ast operator arguments))
           ;; TODO: should check assumptions in *environment*, because we may already have the return value there in some form
           (partial-eval.debug "Immediately evaluating function call to ~A with constant arguments ~A" operator arguments)
           ;; TODO: this assumes the function does not change, let the user decide
           (prog1-bind value
               (make-instance 'constant-form :value (apply operator (constant-values arguments)))
             (partial-eval.debug "Evaluating function call returned ~A" value)))
          ((and (inline-function-call? ast operator arguments)
                (< *function-call-inline-level* +default-function-call-inline-limit+))
           (bind ((source (make-function-lambda-form operator))
                  (*function-call-inline-level* (1+ *function-call-inline-level*)))
             (restart-case
                 (if source
                     (bind ((*environment* (clone-environment))
                            (lambda-ast (walk-form source)))
                       (bind ((*print-level* 3))
                         (partial-eval.debug "Inlining function call to ~A as ~A" operator source))
                       (partial-eval-lambda-list (arguments-of lambda-ast) arguments)
                       (partial-eval-implicit-progn lambda-ast))
                     (make-free-application-form operator arguments))
               (give-up nil ast))))
          (t
           (partial-eval.debug "Partial evaluating function call to ~A with arguments ~A" operator arguments)
           (partial-eval-function-call ast operator arguments)))))

(def layered-method partial-eval-form ((ast multiple-value-call-form))
  (bind ((arguments (mapcar #'partial-eval-form (arguments-of ast))))
    (partial-eval-lambda-list (arguments-of (function-designator-of ast)) arguments)
    (partial-eval-form (function-designator-of ast))))

(def layered-method partial-eval-form ((ast macrolet-form))
  (partial-eval-implicit-progn ast))

(def layered-method partial-eval-form ((ast flet-form))
  ;; TODO: really?
  (extend-bindings (mapcar (lambda (binding)
                             (make-instance 'lexical-variable-binding-form
                                            :name (name-of binding)
                                            :initial-value binding))
                           (bindings-of ast)))
  (partial-eval.debug "Evaluating flet function ~A" ast)
  (partial-eval-implicit-progn ast))

(def layered-method partial-eval-form ((ast labels-form))
  (partial-eval-implicit-progn ast))

(def layered-method partial-eval-form ((ast lexical-application-form))
  (bind ((*environment* (clone-environment))
         (lambda-ast (definition-of ast))
         (argument-values (mapcar #'partial-eval-form (arguments-of ast))))
    (partial-eval.debug "Lexical function application ~A for arguments ~A with values ~A"
                        (operator-of ast) (arguments-of lambda-ast) argument-values)
    (partial-eval-lambda-list (arguments-of lambda-ast) argument-values)
    (partial-eval-implicit-progn lambda-ast)))

(def layered-method partial-eval-form ((ast lambda-application-form))
  (bind ((*environment* (clone-environment))
         (argument-definitions (arguments-of (operator-of ast)))
         (argument-values (mapcar #'partial-eval-form (arguments-of ast))))
    (partial-eval.debug "Lambda function application ~A for arguments ~A with values ~A"
                        (operator-of ast) argument-definitions argument-values)
    (partial-eval-lambda-list argument-definitions argument-values)
    (partial-eval-implicit-progn (operator-of ast))))

(def layered-method partial-eval-form ((ast the-form))
  (partial-eval-form (value-of ast)))

(def layered-method partial-eval-form ((ast locally-form))
  (partial-eval-implicit-progn ast))

(def layered-method partial-eval-form ((ast symbol-macrolet-form))
  ;; TODO: bindings
  (partial-eval-implicit-progn ast))

;;;;;;
;;; partial-eval

(def (function e) partial-eval (form &key (layer 'standard-partial-eval-layer)
                                     assumptions bindings types
                                     eval-function-calls inline-function-calls lookup-variable-values)
  "The function PARTIAL-EVAL takes a lisp FORM and returns another lisp FORM. The resulting form should, in all possible environments, produce the same return value(s), the same side effects in the same order, and the same non local exits (in and out), as the original FORM would have produced. The ENVIRONMENT parameter specifies the initial assumptions in which the form should be partially evaluated. The LAYER parameter provides a way to customize the standard partial evaluation logic to your needs."
  (bind ((*environment* (make-instance 'environment
                                       :assumptions assumptions
                                       :bindings bindings
                                       :types types))
         (*function-call-inline-level* 0))
    (with-active-layers (ignore-undefined-references)
      (funcall-with-layer-context (aprog1
                                      (if layer
                                          (adjoin-layer layer (current-layer-context))
                                          (current-layer-context))
                                    ;; KLUDGE: unfortunately contextl does not support slot values in layers
                                    ;; TODO this is not thread safe
                                    (bind ((prototype (contextl::layer-context-prototype it)))
                                      (setf (eval-function-calls-of prototype) eval-function-calls
                                            (inline-function-calls-of prototype) inline-function-calls
                                            (lookup-variable-values-of prototype) lookup-variable-values)))
                                  (lambda ()
                                    (unwalk-form (partial-eval-form (walk-form form))))))))
