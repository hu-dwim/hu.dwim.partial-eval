;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval-test)

;;;;;;
;;; Test

(defun test (&optional (function-name 'make-instance))
  (compile nil (print (make-generic-function-lambda-form (fdefinition function-name)))))

(defun read-this-source ()
  (read-source (cl-partial-eval-system::system-relative-pathname :cl-partial-eval-test "/test/partial-eval.lisp")))

(defmethod foo (i)
  (print "t"))

(defmethod foo ((i integer))
  (print "integer")
  (call-next-method))

#+nil
(defmethod foo :after (i)
  (print "after"))

;;;;;;
;;; Loop based compare

(declaim (inline compare))

(defun compare (text-1 text-2)
  "A simple text comparison."
  (and (= (length text-1)
          (length text-2))
       (loop
          for char-1 :across text-1
          for char-2 :across text-2
          do (unless (char= char-1 char-2)
               (return nil))
          finally (return t))))

(def test test/compare/inline ()
  (disassemble '(lambda (text)
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (compare "he" text))))

(def test test/compare/partial-eval ()
  (is (equal (partial-eval '(lambda (text-2)
                              (compare "he" text-2)))
             '(lambda (text-2)
                (if (= 2 (length text-2))
                    (block nil
                      (if (char= #\h (aref text-2 0))
                          nil
                          (return-from nil nil))
                      (if (char= #\e (aref text-2 1))
                          nil
                          (return-from nil nil))
                      (return-from nil t)))))))

;;;;;;
;;; Recursion based compare

(defun compare* (text-1 text-2)
  "A simple text comparison."
  (bind ((l (length text-1))
         (i 0))
    (labels ((%compare (text-1 text-2)
               (if (= l i)
                   t
                   (when (char= (aref text-1 i)
                                (aref text-2 i))
                     (incf i)
                     (%compare text-1 text-2)))))
      (and (= l (length text-2))
           (%compare text-1 text-2)))))

;;;;;;
;;; Append*

(declaim (inline append*))

(defun append* (list-1 list-2)
  "A recursive append for lists."
  (if (consp list-1)
      (cons (car list-1)
            (append* (cdr list-1) list-2))
      list-2))

(def test test/append*/inline ()
  (disassemble '(lambda (list)
                  (declare (optimize (speed 3) (debug 0) (safety 0)))
                  (append* '(1 2 3) list))))

(def test test/append*/partial-eval ()
  (is (equal (partial-eval '(lambda (list)
                              (append* '(1 2 3) list)))
             '(lambda (list)
                (cons 1 (cons 2 (cons 3 list)))))))

;;;;;;
;;; Power

(declaim (inline power))

(defun power (base exponent)
  "The usual power function"
  (loop with result = 1
     repeat exponent
     do (setf result (* base result))
     finally (return result)))

(def test test/power/inline ()
  (disassemble '(lambda (base)
                  (declare (optimize (speed 3) (debug 0) (safety 0)))
                  (power base 4))))

(def test test/power/partial-eval ()
  (is (equal (partial-eval '(lambda (base)
                              (power base 4)))
             '(lambda (base)
                (* base (* base (* base (* base 1))))))))

;;;;;;
;;; Match

(declaim (inline match))

(defun match (expression text)
  "A very simple matcher that takes EXPRESSION as a sequence of alphanumeric characters, (), *, + and ? with the usual regular expression semantics.
Returns an index into TEXT specifying the next character that was not matched by expression or NIL if no prefix of TEXT matches."
  (labels ((%match (expression text)
             (bind ((text-length (length text))
                    (expression-length (length expression))
                    (text-index 0)
                    (expression-index 0))
               (loop
                  (unless (< expression-index expression-length)
                    (return text-index))
                  (when (> text-index text-length)
                    (return nil))
                  (bind ((text-char (when (< text-index text-length)
                                      (elt text text-index)))
                         (expression-char (elt expression expression-index))
                         (subexpression? (eq expression-char #\())
                         (match-expression-begin expression-index)
                         (match-expression-end (1+ (if subexpression?
                                                       (position #\) expression :start expression-index)
                                                       expression-index)))
                         (match-expression-length (- match-expression-end match-expression-begin))
                         (cardinality (when (< match-expression-end expression-length)
                                        (elt expression match-expression-end)))
                         (match-index (if subexpression?
                                          (%match (subseq expression (1+ match-expression-begin) (1- match-expression-end))
                                                  (subseq text text-index))
                                          (if (eq expression-char text-char)
                                              1
                                              nil))))
                    (case cardinality
                      (#\?
                       (when match-index
                         (incf text-index match-index))
                       (incf expression-index (1+ match-expression-length)))
                      (#\*
                       (if match-index
                           (incf text-index match-index)
                           (incf expression-index (1+ match-expression-length))))
                      (t
                       (if match-index
                           (progn
                             (incf text-index match-index)
                             (incf expression-index match-index))
                           (return nil)))))))))
    (declare (optimize (speed 3) (debug 0) (safety 0)))
    (%match expression text)))

(def test test/match/inline ()
  (disassemble '(lambda (text)
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (match "a(ab)*b" text))))

(def test test/match/call ()
  (bind ((expression "a(ab)*b"))
    (is (not (match expression "")))
    (is (not (match expression "a")))
    (is (not (match expression "aa")))
    (is (not (match expression "b")))
    (is (not (match expression "bb")))
    (is (= 2 (match expression "ab")))
    (is (= 4 (match expression "aabb")))
    (is (= 6 (match expression "aababb")))
    (is (= 2 (match expression "abab")))))

(def test test/match/partial-eval ()
  (bind ((form (partial-eval '(match "a(ab)*b" text))))
    (is (eval form))
    (is (eval form))))
