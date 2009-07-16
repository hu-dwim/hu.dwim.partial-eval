(in-package :cl-partial-eval)

;;;;;;
;;; Source provider

(def (special-variable e) *sources* (make-hash-table :test #'equal))

(def (function e) read-source (file-name)
  (setf (gethash file-name *sources*)
        (with-open-file (src file-name)
          (iter (for item = (source-text:source-read src nil src nil nil))
                (until (eq item src))
                (collect item)
                (until (typep item 'source-text:source-lexical-error))))))

(def (function e) read-pcl-sources ()
  (clrhash *sources*)
  (iter (with *package* = (find-package :sb-pcl))
        (for file-name :in (directory "/home/levy/workspace/sbcl/src/pcl/*.lisp"))
        (read-source file-name)))

(def (generic e) make-lisp-form (node)
  (:method ((node source-text:source-semicolon-comment))
    (values))

  (:method ((node source-text:source-number))
    (source-text:source-number-value node))

  (:method ((node source-text:source-string))
    (source-text:source-string-value node))

  (:method ((node source-text:source-list))
    (mapcar 'make-lisp-form (source-text:source-sequence-elements node)))

  (:method ((node source-text:source-symbol))
    (source-text:source-symbol-value node))

  (:method ((node source-text:source-subform))
    (make-lisp-form (source-text:source-object-subform node)))

  (:method ((node source-text:source-quote))
    (list 'quote (call-next-method)))

  (:method ((node source-text:source-function))
    (list 'function (call-next-method))))

(def (function e) find-function-source (function-name)
  (iter (for (file-name source) :in-hashtable *sources*)
        (iter (for node :in source)
              (when (typep node 'source-text:source-list)
                (bind ((elements (source-text:source-sequence-elements node))
                       (first-node (first elements))
                       (second-node (second elements)))
                  (when (and (typep first-node 'source-text:source-symbol)
                             (eq 'defun (source-text:source-symbol-value first-node))
                             (typep second-node 'source-text:source-symbol)
                             (eq function-name (source-text:source-symbol-value second-node)))
                    (return-from find-function-source node)))))))

(def (function e) make-function-lambda-form (function-name)
  (awhen (find-function-source function-name)
    `(lambda ,@(cddr (make-lisp-form it)))))

;; TODO: use the one in walker
(def function split-function-lambda-list (lambda-list)
  (iter (for argument-cell :on lambda-list)
        (for argument = (car argument-cell))
        (when (member argument lambda-list-keywords :test #'eq)
          (return (values arguments argument-cell )))
        (collect argument :into arguments)
        (finally (return (values arguments nil)))))

(def function make-generic-method-lambda-form (method)
  (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-pcl:method-lambda-list method)))
         (source (slot-value method 'sb-pcl::source))
         (namestring (slot-value source 'namestring))
         (toplevel-form-number (slot-value source 'sb-c::toplevel-form-number))
         (pathname (make-pathname :device nil :defaults (translate-logical-pathname namestring)))
         (source (gethash pathname *sources*)))
    (iter (with index = 0)
          (for node :in source)
          (when (typep node 'source-text:source-list)
            (when (= index toplevel-form-number)
              (return (bind ((form (make-lisp-form node)))
                        (with-unique-names (arguments methods)
                          `(lambda (,arguments ,methods)
                             (bind ((,(append required-arguments other-arguments?) ,arguments))
                               ,@(nthcdr (1+ (position-if #'consp form)) form)))))))
            (incf index))
          (finally (error "Cannot find source for ~A" method)))))

(def function make-generic-function-discriminating-form (gf arguments-list)
  (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-mop:generic-function-lambda-list gf)))
         (sorted-methods (sb-pcl::sort-applicable-methods
                          (sb-pcl::compute-precedence (sb-mop:generic-function-lambda-list gf)
                                                      (length required-arguments)
                                                      (sb-mop:generic-function-argument-precedence-order gf))
                          (copy-list (sb-mop:generic-function-methods gf))
                          (make-list (length required-arguments) :initial-element t)))
         (methods-info (mapcar (lambda (method)
                                 (list method (gensym "METHOD")))
                               sorted-methods))
         (counter 0)
         (effective-methods-table (make-hash-table))
         (compiled-net (sb-pcl::generate-discrimination-net-internal
                        gf sorted-methods nil
                        (lambda (methods known-types)
                          (declare (ignore known-types))
                          (when methods
                            (bind ((id (incf counter))
                                   (method (first methods)))
                              (setf (gethash method effective-methods-table) id)))
                          `(sb-pcl::methods ,methods))
                        (lambda (position type true-value false-value)
                          (bind ((arg (elt required-arguments position)))
                            (if (eq (car type) 'eql)
                                (bind ((false-case? (and (consp false-value)
                                                         (or (eq (car false-value)
                                                                 'sb-pcl::scase)
                                                             (eq (car false-value)
                                                                 'sb-pcl::mcase))
                                                         (eq arg (cadr false-value))))
                                       (false-clauses (if false-case?
                                                          (cddr false-value)
                                                          `((t ,false-value))))
                                       (case-sym (if (and (sb-pcl::dnet-methods-p true-value)
                                                          (if false-case?
                                                              (eq (car false-value)
                                                                  'sb-pcl::mcase)
                                                              (sb-pcl::dnet-methods-p
                                                               false-value)))
                                                     'sb-pcl::mcase
                                                     'sb-pcl::scase))
                                       (type-sym `(,(cadr type))))
                                  `(,case-sym ,arg
                                              (,type-sym ,true-value)
                                              ,@false-clauses))
                                `(if ,(bind ((arg (elt required-arguments position)))
                                            (case (car type)
                                              (class (bind ((class (second type)))
                                                       (if (null (class-direct-subclasses class))
                                                           `(sb-pcl::class-eq-test ,arg ,class)
                                                           `(sb-pcl::class-test ,arg ,class))))
                                              (class-eq `(sb-pcl::class-eq-test ,arg ,(cadr type)))))
                                     ,true-value
                                     ,false-value))))
                        #'identity))
         effective-methods)
    (flet ((listify-table (table)
             (bind ((result (mapcar 'car (sort (bind (list)
                                                 (maphash (lambda (k v)
                                                            (push (cons k v)
                                                                  list))
                                                          table)
                                                 list)
                                               '<
                                               :key #'cdr))))
               result)))
      (setf effective-methods (listify-table effective-methods-table)))
    (unless effective-methods
      (sb-c:compiler-style-warn "No effective method found for ~S~%" gf))
    `(flet ,(mapcar (lambda (method-info)
                      (bind ((form (make-generic-method-lambda-form (car method-info)))
                             (arguments (second form)))
                        `(,(second method-info) ,arguments
                           (flet ((call-next-method ()
                                    (apply (first ,(second arguments)) ,(first arguments))))
                             ,@(cddr form)))))
                    methods-info)
       (macrolet ((sb-pcl::methods (methods)
                    (bind ((method (first methods))
                           (name (second (find method ',methods-info :key 'car)))
                           (arguments ',required-arguments))
                      (if (null method)
                          (if (not ',other-arguments?)
                              (list* 'no-applicable-method ',gf arguments)
                              (list* 'apply ''no-applicable-method ',gf (append arguments '(.rest.))))
                          `(,name ,',arguments-list ',(mapcar (lambda (method)
                                                                (second (find method ',methods-info :key 'car)))
                                                              (rest methods)))))))
         ,compiled-net))))

(def (function e) make-generic-function-lambda-form (function-name)
  (bind ((generic-function (when (ignore-errors (fboundp function-name))
                             (fdefinition function-name))))
    (when (typep generic-function 'standard-generic-function)
      (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-mop:generic-function-lambda-list generic-function)))
             (rest-argument (when other-arguments? '.rest.)))
        (with-unique-names (arguments-list)
          `(lambda (,@required-arguments ,@(when rest-argument `(&rest ,rest-argument)))
             (bind ((,arguments-list (list* ,@required-arguments ,rest-argument)))
               ,(make-generic-function-discriminating-form generic-function arguments-list))))))))