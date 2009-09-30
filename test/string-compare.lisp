;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; string-compare-with-loop

(def suite* (test/string-compare-with-loop :in test))

(def function string-compare-with-loop (text-1 text-2)
  "A simple text comparison."
  (and (= (length text-1)
          (length text-2))
       (loop
          :for char-1 :across text-1
          :for char-2 :across text-2
          :do (unless (char= char-1 char-2)
                (return nil))
          :finally (return t))))

;;;;;;
;;; correctness

(def test test/string-compare-with-loop/correctness ()
  (is (string-compare-with-loop "hello" "hello"))
  (is (not (string-compare-with-loop "hello" "he")))
  (is (not (string-compare-with-loop "he" "hello")))
  (is (not (string-compare-with-loop "" "hello")))
  (is (not (string-compare-with-loop "hello" ""))))

;;;;;;
;;; partial-eval

(def layer string-compare-with-loop ()
  ())

(def layered-method eval-function-call? :in string-compare-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(length))))

(def layered-method inline-function-call? :in string-compare-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-loop))))

(def test test/string-compare-with-loop/partial-eval ()
  (is (equal (partial-eval '(string-compare-with-loop "he" text) :layer 'string-compare-with-loop)
             '(if (= 2 (length text))
               (block nil
                 (if (char= #\h (aref text 0))
                     nil
                     (return-from nil nil))
                 (if (char= #\e (aref text 1))
                     nil
                     (return-from nil nil))
                 (return-from nil t))))))

;;;;;;
;;; string-compare-with-recursion

(def suite* (test/string-compare-with-recursion :in test))

(def function string-compare-with-recursion (text-1 text-2)
  "A simple text comparison."
  (bind ((l (length text-1))
         (i 0))
    (labels ((recurse (text-1 text-2)
               (if (= l i)
                   t
                   (when (char= (aref text-1 i)
                                (aref text-2 i))
                     (incf i)
                     (recurse text-1 text-2)))))
      (and (= l (length text-2))
           (recurse text-1 text-2)))))

;;;;;;
;;; correctness

(def test test/string-compare-with-recursion/correctness ()
  (is (string-compare-with-recursion "hello" "hello"))
  (is (not (string-compare-with-recursion "hello" "he")))
  (is (not (string-compare-with-recursion "he" "hello")))
  (is (not (string-compare-with-recursion "" "hello")))
  (is (not (string-compare-with-recursion "hello" ""))))

;;;;;;
;;; partial-eval

(def layer string-compare-with-recursion ()
  ())

(def layered-method eval-function-call? :in string-compare-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(length))))

(def layered-method inline-function-call? :in string-compare-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-recursion))))

(def test test/string-compare-with-recursion/partial-eval ()
  (is (equal (partial-eval '(string-compare-with-recursion "he" text) :layer 'string-compare-with-recursion)
             nil)))
