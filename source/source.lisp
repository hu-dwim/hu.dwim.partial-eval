;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;;;;;
;;; Source database

(def special-variable *sources* (make-hash-table))

(def function clear-source-cache ()
  (clrhash *sources*))

(def function read-source-form (fdefinition)
  ;; KLUDGE: all this hassle is to workaround SBCL's bootstrapping package names
  (bind ((original-find-package (fdefinition 'find-package))
         (temporary-package (make-package "TEMPORARY")))
    (unwind-protect
         (progn
           (handler-bind ((package-error #'continue))
             (setf (fdefinition 'find-package) (lambda (designator)
                                                 (cond ((equal "SB!XC" designator)
                                                        temporary-package)
                                                       ((stringp designator)
                                                        (funcall original-find-package (substitute #\- #\! designator)))
                                                       (t (funcall original-find-package designator))))))
           (bind ((definition-source (sb-introspect:find-definition-source fdefinition))
                  (pathname (sb-introspect:definition-source-pathname definition-source))
                  (source-path (sb-introspect:definition-source-form-path definition-source))
                  (first-index (car source-path)))
             (with-open-file (stream pathname)
               (bind ((*readtable* (swank-backend::shebang-readtable))
                      (*package* (find-package :common-lisp-user)))
                 (iter (for index :from 0)
                       (for form = (handler-bind ((sb-int::simple-reader-package-error #'continue))
                                     (read stream #f stream)))
                       (until (eq form stream))
                       (when (eq 'in-package (first form))
                         (setf *package* (find-package (second form)))
                         (awhen (cdr (assoc (package-name *package*) swank:*readtable-alist* :test 'string=))
                           (setf *readtable* it)))
                       (when (= first-index index)
                         (return form)))))))
      (handler-bind ((package-error #'continue))
        (setf (fdefinition 'find-package) original-find-package)
        (delete-package temporary-package)))))

;;;;;;
;;; Generic function

;; TODO: use the one in alexandria
(def function split-function-lambda-list (lambda-list)
  (iter (for argument-cell :on lambda-list)
        (for argument = (car argument-cell))
        (when (member argument lambda-list-keywords :test #'eq)
          (return (values arguments argument-cell )))
        (collect argument :into arguments)
        (finally (return (values arguments nil)))))

(def function make-generic-method-lambda-form (method)
  (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-pcl:method-lambda-list method)))
         (form (read-source-form method)))
    (with-unique-names (arguments methods)
      `(lambda (,arguments ,methods)
         (bind ((,(append required-arguments other-arguments?) ,arguments))
           ,@(nthcdr (1+ (position-if #'consp form)) form))))))

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

;;;;;;
;;; Function

(def generic make-function-lambda-form (function)
  (:method :around (function)
    (or (gethash function *sources*)
        (setf (gethash function *sources*)
              (call-next-method function))))

  (:method ((name symbol))
    (make-function-lambda-form (fdefinition name)))

  (:method ((function function))
    (bind (((:values nil nil function-name) (function-lambda-expression function))
           (form (read-source-form function)))
      (cond ((and (eq 'defun (first form))
                  (eq function-name (second form)))
             ;; TODO: use walker
             (bind (((:values body declarations nil) (parse-body (cdddr form) :documentation #t)))
               `(lambda ,(caddr form)
                  ,@declarations
                  ,@body)))
            ((and (eq 'def (first form))
                  (eq 'function (second form))
                  (eq function-name (third form)))
             (bind (((:values body declarations nil) (parse-body (cddddr form) :documentation #t)))
               `(lambda ,(cadddr form)
                  ,@declarations
                  ,@body)))
            (t nil))))

  (:method ((function generic-function))
    (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-mop:generic-function-lambda-list function)))
           (rest-argument (when other-arguments? '.rest.)))
      (with-unique-names (arguments-list)
        `(lambda (,@required-arguments ,@(when rest-argument `(&rest ,rest-argument)))
           (bind ((,arguments-list (list* ,@required-arguments ,rest-argument)))
             ,(make-generic-function-discriminating-form function arguments-list)))))))
