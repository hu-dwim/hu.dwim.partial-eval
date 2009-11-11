;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; test/match-simple-regexp

(def suite* (test/match-simple-regexp :in test))

(def function match-simple-regexp (expression text)
  "A very simple matcher that takes EXPRESSION as a sequence of alphanumeric characters, (), *, + and ? with the usual regular expression semantics. Returns the position in TEXT specifying the next character that was not matched by EXPRESSION, or NIL if there's no match at all."
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

(def test test/match-simple-regexp/correctness ()
  (bind ((expression '("a" (* "ab") "b")))
    (is (not (match-simple-regexp expression "")))
    (is (not (match-simple-regexp expression "a")))
    (is (not (match-simple-regexp expression "aa")))
    (is (not (match-simple-regexp expression "b")))
    (is (not (match-simple-regexp expression "bb")))
    (is (= 2 (match-simple-regexp expression "ab")))
    (is (= 4 (match-simple-regexp expression "aabb")))
    (is (= 6 (match-simple-regexp expression "aababb")))
    (is (= 2 (match-simple-regexp expression "abab")))))

(def layer match-simple-regexp-layer (standard-partial-eval-layer)
  ())

(def layered-method inline-function-call? :in match-simple-regexp-layer ((ast free-application-form) operator arguments)
  (or (call-next-layered-method)
      (member operator '(match-simple-regexp))))

(def test test/match-simple-regexp/partial-eval ()
  (with-active-layers (match-simple-regexp-layer)
    (is (equal (partial-eval '(match-simple-regexp '("a" (* "ab") "b") text))
               nil))))
