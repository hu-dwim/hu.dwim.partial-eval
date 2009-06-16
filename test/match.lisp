;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-partial-eval-test)

;;;;;;
;;; Match

(def function-with-soucre match (expression text)
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
    (%match expression text)))

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


#|
;; TODO: kill this

(def function-with-soucre alma (a b)
  ""
  (let* ((x (length a))
         (z 0))
    (loop
       repeat x
       do (incf z b))
    z))

#+nil
(let* ((x (length a)) (z 0))
  (let ((repeat x))
    (tagbody
     sb-loop::next-loop
       (if (<= repeat 0)
           (go sb-loop::end-loop)
           (setq repeat (1- repeat)))
       (setq z (+ z b))
       (go sb-loop::next-loop)
     sb-loop::end-loop))
  z)

;; (alma "Hello" 5) -> 25
|#