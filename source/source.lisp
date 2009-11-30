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
  ;; KLUDGE: all this hassle is to workaround SBCL's bootstrapping package names, this is obviously non thread safe
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
                       (when (member (first form) '(common-lisp:in-package hu.dwim.common:in-package))
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
         (form (cddr (read-source-form method))))
    (with-unique-names (arguments methods)
      `(lambda (,arguments ,methods)
         (destructuring-bind ,(append required-arguments other-arguments?) ,arguments
           ;; KLUDGE: TODO: this is pretty much broken
           ,@(bind ((form (remove-if (lambda (element)
                                       (member element '(:before :around :after)))
                                     form)))
                   (nthcdr (1+ (position-if #'consp form)) form)))))))

(def function make-generic-function-discriminating-form (function arguments-list)
  (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-mop:generic-function-lambda-list function)))
         (sorted-applicable-methods (sb-pcl::sort-applicable-methods
                                     (sb-pcl::compute-precedence (sb-mop:generic-function-lambda-list function)
                                                                 (length required-arguments)
                                                                 (sb-mop:generic-function-argument-precedence-order function))
                                     (copy-list (sb-mop:generic-function-methods function))
                                     (make-list (length required-arguments) :initial-element t)))
         (methods-info (mapcar (lambda (method)
                                 (list method (gensym "METHOD")))
                               sorted-applicable-methods))
         (argument-names (iter (for index :from 0 :below 10)
                               (collect (sb-pcl::dfun-arg-symbol index))))
         (discrimination-net (sb-pcl::generate-discrimination-net function sorted-applicable-methods nil #t))
         (effective-methods-form (map-form discrimination-net
                                           (lambda (form)
                                             (if (and (consp form)
                                                      (eq 'sb-pcl::methods (first form)))
                                                 (compute-effective-method function (generic-function-method-combination function) (second form))
                                                 form))))
         (expanded-call-methods-form (map-form effective-methods-form
                                               (lambda (form)
                                                 (if (and (consp form)
                                                          (eq 'call-method (first form)))
                                                     (bind ((method (second form))
                                                            (next-methods (third form))
                                                            (name (second (find method methods-info :key 'car))))
                                                       `(,name ,arguments-list (list ,@(mapcar (lambda (method)
                                                                                                 `(function ,(or (second (find method methods-info :key 'car))
                                                                                                                 method)))
                                                                                               next-methods))))
                                                     (if (member form argument-names)
                                                         (elt required-arguments (position form argument-names))
                                                         form)))))
         (method-forms (mapcar (lambda (method-info)
                                 (bind ((form (make-generic-method-lambda-form (car method-info)))
                                        (arguments (second form)))
                                   `(,(second method-info) ,arguments
                                      (flet ((call-next-method ()
                                               (funcall (car ,(second arguments)) ,(first arguments) (cdr ,(second arguments)))))
                                        ,@(cddr form)))))
                               methods-info))
         (make-method-form-names (make-hash-table))
         (make-method-forms (prog1-bind functions nil
                              (map-form expanded-call-methods-form
                                        (lambda (form)
                                          (if (and (consp form)
                                                   (eq 'make-method (first form)))
                                              (bind ((name (gensym "METHOD")))
                                                (setf (gethash form make-method-form-names) name)
                                                (push `(,name (&rest args)
                                                              (declare (ignore args))
                                                              ,(second form)) functions))
                                              form))))))
    (declare (ignore other-arguments?))
    `(flet ,method-forms
       (flet ,make-method-forms
         ,(map-form expanded-call-methods-form
                    (lambda (form)
                      (if (and (consp form)
                               (eq 'make-method (first form)))
                          (gethash form make-method-form-names)
                          form)))))))

(def function map-form (form function)
  (labels ((recurse (form)
             (setf form (funcall function form))
             (if (consp form)
                 (cons (recurse (car form))
                       (recurse (cdr form)))
                 form)))
    (recurse form)))

;;;;;;
;;; Function

(def generic make-function-lambda-form (function)
  (:method :around (function)
    (or (gethash function *sources*)
        (setf (gethash function *sources*)
              (call-next-method function))))

  (:method ((name symbol))
    (make-function-lambda-form (fdefinition name)))

  (:method ((name cons))
    (make-function-lambda-form (fdefinition name)))

  (:method ((function function))
    (bind (((:values nil nil function-name) (function-lambda-expression function))
           (form (read-source-form function)))
      (cond ((and (eq 'defun (first form))
                  (equal function-name (second form)))
             ;; TODO: use walker
             (bind (((:values body declarations nil) (parse-body (cdddr form) :documentation #t)))
               `(lambda ,(caddr form)
                  ,@declarations
                  ,@body)))
            ((and (eq 'def (first form))
                  (eq 'function (second form))
                  (equal function-name (third form)))
             (bind (((:values body declarations nil) (parse-body (cddddr form) :documentation #t)))
               `(lambda ,(cadddr form)
                  ,@declarations
                  ,@body)))
            (t nil))))

  (:method ((function generic-function))
    (bind (((:values required-arguments other-arguments?) (split-function-lambda-list (sb-mop:generic-function-lambda-list function)))
           (rest-argument (when other-arguments? 'rest)))
      (with-unique-names (arguments-list)
        `(lambda (,@required-arguments ,@(when rest-argument `(&rest ,rest-argument)))
           (bind ((,arguments-list (list* ,@required-arguments ,rest-argument)))
             ,(make-generic-function-discriminating-form function arguments-list)))))))
