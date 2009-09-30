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

(def layer string-compare-with-loop-layer (standard-partial-eval-layer)
  ())

(def layered-method eval-function-call? :in string-compare-with-loop-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(aref))))

(def layered-method inline-function-call? :in string-compare-with-loop-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-loop))))

(def layered-method %partial-eval :in string-compare-with-loop-layer ((ast free-application-form))
  (aif (and (eq 'length (operator-of ast))
            (find (unwalk-form (make-instance 'free-application-form
                                              :operator 'length
                                              :arguments (mapcar #'%partial-eval (arguments-of ast))))
                  (hu.dwim.partial-eval::assumptions-of hu.dwim.partial-eval::*environment*) :test (lambda (a b) (member a b :test 'equal))))
       (make-instance 'constant-form :value (second it))
       (call-next-method)))

(def test test/string-compare-with-loop/partial-eval ()
  (with-active-layers (string-compare-with-loop-layer)
    (is (equal t (partial-eval '(string-compare-with-loop "he" "he"))))
    (is (equal nil (partial-eval '(string-compare-with-loop "he" "hello"))))
    (is (equal (partial-eval '(string-compare-with-loop "" text))
               '(if (= 0 (length text))
                    t)))
    (is (equal (partial-eval '(string-compare-with-loop "he" text))
               '(if (= 2 (length text))
                 (block nil
                   (if (char= #\h (aref text 0))
                       nil
                       (return-from nil nil))
                   (if (char= #\e (aref text 1))
                       nil
                       (return-from nil nil))
                   (return-from nil t)))))
    (bind ((evaluated-form (partial-eval '(string-compare-with-loop t-1 t-2))))
      (is (equal t (eval `(bind ((t-1 "he")
                                 (t-2 "he"))
                            ,evaluated-form))))
      (is (equal nil (eval `(bind ((t-1 "he")
                                   (t-2 "hi"))
                              ,evaluated-form))))
      (is (equal nil (eval `(bind ((t-1 "he")
                                   (t-2 "hello"))
                              ,evaluated-form)))))))

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

(def layer string-compare-with-recursion-layer (standard-partial-eval-layer)
  ())

(def layered-method eval-function-call? :in string-compare-with-recursion-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(aref))))

(def layered-method inline-function-call? :in string-compare-with-recursion-layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-recursion))))

(def test test/string-compare-with-recursion/partial-eval ()
  (with-active-layers (string-compare-with-recursion-layer)
    (is (equal (partial-eval '(string-compare-with-recursion "" text))
               '(if (= 0 (length text))
                    t)))
    (is (equal (partial-eval '(string-compare-with-recursion "he" text))
               '(if (= 2 (length text))
                    (if (char= #\h (aref text 0))
                        (if (char= #\e (aref text 1))
                            t)))))))
