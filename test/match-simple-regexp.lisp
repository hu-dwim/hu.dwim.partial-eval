;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; match-simple-regexp-with-recursion

(def suite* (test/match-simple-regexp-with-recursion :in test))

(def function match-simple-regexp-with-recursion (expression text)
  "A very simple matcher that takes EXPRESSION as a sequence of alphanumeric characters, (), *, + and ? with the usual regular expression semantics.
Returns the position in TEXT specifying the next character that was not matched by EXPRESSION, or NIL if there's no match at all."
  (let ((text-length (length text)))
    (labels ((recurse (expression text position)
               (if (stringp expression)
                   (loop
                      with expression-length = (length expression)
                      for expression-index :from 0 :below expression-length
                      for text-index :from position
                      when (or (>= text-index text-length)
                               (not (char= (elt expression expression-index)
                                           (elt text text-index))))
                      do (return nil)
                      finally (return expression-index))
                   (let ((head (car expression)))
                     (if (symbolp head)
                         (ecase head
                           (? #+nil (assert (not (cddr expression))) ;; TODO:
                              (or (recurse (second expression) text position)
                                  0))
                           (* #+nil (assert (not (cddr expression))) ;; TODO:
                              (loop
                                 with sum = 0
                                 for length = (recurse (second expression) text position)
                                 do (if length
                                        (progn
                                          (incf position length)
                                          (incf sum length))
                                        (return sum)))))
                         (loop
                            with sum = 0
                            for element :in expression
                            for length = (recurse element text position)
                            do (if length
                                   (progn
                                     (incf position length)
                                     (incf sum length))
                                   (return nil))
                            finally (return sum)))))))
      (recurse expression text 0))))

;;;;;;
;;; correctness

(def test test/match-simple-regexp-with-recursion/correctness ()
  (bind ((expression '("a" (* "ab") "b")))
    (is (not (match-simple-regexp-with-recursion expression "")))
    (is (not (match-simple-regexp-with-recursion expression "a")))
    (is (not (match-simple-regexp-with-recursion expression "aa")))
    (is (not (match-simple-regexp-with-recursion expression "b")))
    (is (not (match-simple-regexp-with-recursion expression "bb")))
    (is (= 2 (match-simple-regexp-with-recursion expression "ab")))
    (is (= 4 (match-simple-regexp-with-recursion expression "aabb")))
    (is (= 6 (match-simple-regexp-with-recursion expression "aababb")))
    (is (= 2 (match-simple-regexp-with-recursion expression "abab")))))

;;;;;;
;;; partial-eval

(def layer match-simple-regexp-with-recursion-layer (standard-partial-eval-layer)
  ())

(def layered-method inline-function-call? :in match-simple-regexp-with-recursion-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(match-simple-regexp-with-recursion))))

(def test test/match-simple-regexp-with-recursion/partial-eval ()
  #+nil
  (with-active-layers (match-simple-regexp-with-recursion-layer)
    (is (equal (partial-eval '(match-simple-regexp-with-recursion "a(ab)*b" text))
               nil))))
