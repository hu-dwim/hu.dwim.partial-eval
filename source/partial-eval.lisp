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
  ((assumptions nil)
   (bindings nil)))

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

(def (layered-function e) eval-function-call? (ast)
  (:documentation "Returns TRUE if the function call should be evaluated at partial eval time, FALSE otherwise.")

  (:method ((ast free-application-form))
    (member (operator-of ast) '(eq eql not null car cdr consp eql first second third fourth getf))))

(def (layered-function e) inline-function-call? (ast)
  (:documentation "Returns TRUE if the function call should be inlined at partial eval time, FALSE otherwise.")

  (:method ((ast free-application-form))
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
    (not (eval-function-call? ast))))

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

  (:method ((ast variable-reference-form))
    nil)

  (:method ((ast free-application-form))
    nil)

  (:method ((ast setq-form))
    nil))

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
    (not (eval-function-call? ast))))

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
           (evaluated-values (eval `(bind ((,@(cdadr (unwalk-form (make-instance 'lambda-function-form
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

(def (layered-function e) %partial-eval (form)
  (:documentation "This function is the recursive variant of PARTIAL-EVAL.")

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
                            :body (body-of evaluated-body)))
            (t evaluated-body))))

  (:method ((ast return-from-form))
    (make-instance 'return-from-form
                   :target-block (target-block-of ast)
                   :result (%partial-eval (result-of ast))))

  (:method ((ast tagbody-form))
    (iter (with body = ast)
          (for count :from 0)
          (when (= count 10)
            (break "Too many unrolling of tagbody go forms, giving up and returning original loop")
            (return ast))
          (bind ((evaluated-body (partial-eval-implicit-progn-body body))
                 (non-local-exits (remove-if-not (lambda (non-local-exit)
                                                   (and (typep non-local-exit 'go-form)
                                                        (eq ast (enclosing-tagbody-of non-local-exit))))
                                                 (collect-potential-non-local-exits evaluated-body))))
            (if (typep evaluated-body 'progn-form)
                (appending (body-of evaluated-body) :into result)
                (collect evaluated-body :into result))
            (when (and (length= 1 non-local-exits)
                       (eq ast (enclosing-tagbody-of (first non-local-exits))))
              (setf body (jump-target-of (first non-local-exits)))
              ;; (break "~A" (princ-to-string (unwalk-form (make-progn-form result))))
              (when body
                (next-iteration)))
            ;; TODO: return tagbody-form when cannot be fully unrolled
            (return
              (aif non-local-exits
                   (make-progn-form (remove-if (of-type 'go-form) result))
                   (make-instance 'constant-form :value nil))))))

  (:method ((ast go-tag-form))
    ast)

  (:method ((ast go-form))
    ast)

  (:method ((ast setq-form))
    (prog1-bind value (%partial-eval (value-of ast))
      (setf (variable-binding (name-of (variable-of ast))) value)))

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
      (cond ((and (eval-function-call? ast)
                  (every (of-type '(or constant-form free-function-object-form)) arguments))
             ;; TODO: should check assumptions in *environment*, because we may already have the return value there
             ;;       or we can infer the return value from the assumptions
             (partial-eval.debug "Immediately evaluating function call to ~A with arguments ~A" operator arguments)
             ;; TODO: this assumes the function do not change, let the user decide
             (prog1-bind value
                 (make-instance 'constant-form
                                :value (apply operator (mapcar (lambda (argument)
                                                                 (etypecase argument
                                                                   (constant-form (value-of argument))
                                                                   (free-function-object-form (name-of argument))))
                                                               arguments)))
               (partial-eval.debug "Evaluating function call returned ~A" value)))
            ((inline-function-call? ast)
             (bind ((source (make-function-lambda-form operator)))
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
                 (give-up nil ast))))
            (t ast))))

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

(def (function e) partial-eval (form &key (environment (make-environment)) layer)
  "The function PARTIAL-EVAL takes a lisp FORM and returns another lisp FORM. The resulting form should,
in all possible circumstances, produce the same return value, the same side effects in the same order,
and the same non local exits, as the original FORM would have produced. The ENVIRONMENT parameter specifies
the initial assumptions in which the form should be evaluated. The LAYER parameter provides a way to customize
the standard partial evaluation logic to your needs."
  (bind ((*environment* environment)
         (contextl::*active-context* contextl::*active-context*))
    (when layer
      (ensure-active-layer layer))
    ;; KLUDGE: shall we really ignore?
    (with-walker-configuration (:undefined-reference-handler nil)
      (unwalk-form (%partial-eval (walk-form form))))))
