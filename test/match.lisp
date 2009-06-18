;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval-test)

;;;;;;
;;; Compare

(declaim (inline compare))

(def function-with-source compare (text-1 text-2)
  "A simple text comparison."
  (and (= (length text-1)
          (length text-2))
       (loop
          for char-1 :across text-1
          for char-2 :across text-2
          do (unless (char= char-1 char-2)
               (return nil))
          finally (return t))))

#|
the compare function's body expands into this:

(if (= (length text-1)
       (length text-2))
    (block nil
      (let ((char-1 nil)
            (loop-across-vector-2097 text-1)
            (loop-across-index-2098 0)
            (loop-across-limit-2099 0))
        (declare (type fixnum loop-across-limit-2099) (type fixnum loop-across-index-2098)
                 (type (or (member nil) vector) loop-across-vector-2097))
        (let ((char-2 nil)
              (loop-across-vector-2100 text-2)
              (loop-across-index-2101 0)
              (loop-across-limit-2102 0))
          (declare (type fixnum loop-across-limit-2102) (type fixnum loop-across-index-2101)
                   (type (or (member nil) vector) loop-across-vector-2100))
          (tagbody
             (setq loop-across-limit-2099 (length loop-across-vector-2097))
             (setq loop-across-limit-2102 (length loop-across-vector-2100))
           next-loop
             (if (>= loop-across-index-2098 loop-across-limit-2099)
                 (progn (go end-loop))
                 nil)
             (setq char-1 (aref loop-across-vector-2097 loop-across-index-2098))
             (setq loop-across-index-2098 (1+ loop-across-index-2098))
             (if (>= loop-across-index-2101 loop-across-limit-2102)
                 (progn (go end-loop))
                 nil)
             (setq char-2 (aref loop-across-vector-2100 loop-across-index-2101))
             (setq loop-across-index-2101 (1+ loop-across-index-2101))
             (if (char= char-1 char-2)
                 nil
                 (progn (return-from nil nil)))
             (go next-loop)
           end-loop
             (return-from nil t))))))
|#

(def test test/compare/inline ()
  (disassemble '(lambda (text)
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (compare "he" text))))

(def test test/compare/partial-eval ()
  (is (equal (partial-eval '(lambda (text-2)
                              (compare "he" text-2)))
             '(lambda (text-2)
                (declare (optimize (speed 3) (safety 0) (debug 0))
                         (type simple-string text))
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
;;; Append*

(declaim (inline append*))

(def function-with-source append* (list-1 list-2)
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
;;; Match

(declaim (inline match))

(def function-with-source match (expression text)
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
