;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; string-compare-with-loop

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

(def layer string-compare-with-loop ()
  ())

(def layered-method eval-function-call? :in string-compare-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(length))))

(def layered-method inline-function-call? :in string-compare-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-loop))))

(def test test/string-compare-with-loop/correctness ()
  (is (string-compare-with-loop "hello" "hello"))
  (is (not (string-compare-with-loop "hello" "he")))
  (is (not (string-compare-with-loop "he" "hello")))
  (is (not (string-compare-with-loop "" "hello")))
  (is (not (string-compare-with-loop "hello" ""))))

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

(def layer string-compare-with-recursion ()
  ())

(def layered-method eval-function-call? :in string-compare-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(length))))

(def layered-method inline-function-call? :in string-compare-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(string-compare-with-recursion))))

(def test test/string-compare-with-recursion/correctness ()
  (is (string-compare-with-recursion "hello" "hello"))
  (is (not (string-compare-with-recursion "hello" "he")))
  (is (not (string-compare-with-recursion "he" "hello")))
  (is (not (string-compare-with-recursion "" "hello")))
  (is (not (string-compare-with-recursion "hello" ""))))

(def test test/string-compare-with-recursion/partial-eval ()
  (is (equal (partial-eval '(string-compare-with-recursion "he" text) :layer 'string-compare-with-recursion)
             nil)))

;;;;;;
;;; list-append-with-recursion

(def function list-append-with-recursion (list-1 list-2)
  "A recursive variant for list append."
  (if (consp list-1)
      (cons (car list-1)
            (list-append-with-recursion (cdr list-1) list-2))
      list-2))

(def layer list-append-with-recursion ()
  ())

(def layered-method eval-function-call? :in list-append-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(car cdr))))

(def layered-method inline-function-call? :in list-append-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(list-append-with-recursion))))

(def test test/list-append-with-recursion/correctness ()
  (is (equal nil (list-append-with-recursion nil nil)))
  (is (equal '(1 2 3) (list-append-with-recursion '(1 2 3) nil)))
  (is (equal '(1 2 3) (list-append-with-recursion nil '(1 2 3))))
  (is (equal '(1 2 3 4 5 6) (list-append-with-recursion '(1 2 3) '(4 5 6)))))

(def test test/list-append-with-recursion/partial-eval ()
  (is (equal (partial-eval '(list-append-with-recursion '(1 2 3) list) :layer 'list-append-with-recursion)
             '(cons 1 (cons 2 (cons 3 list))))))

;;;;;;
;;; integer-power-with-loop

(def function integer-power-with-loop (base exponent)
  "The usual power function with integer EXPONENT."
  (loop
     :with result = 1
     :repeat exponent
     :do (setf result (* base result))
     :finally (return result)))

(def layer integer-power-with-loop ()
  ())

(def layered-method eval-function-call? :in integer-power-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(<= - ceiling))))

(def layered-method inline-function-call? :in integer-power-with-loop ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(integer-power-with-loop))))

(def test test/integer-power-with-loop/correctness ()
  (is (= 1 (integer-power-with-loop 10 0)))
  (is (= 10 (integer-power-with-loop 10 1)))
  (is (= 100 (integer-power-with-loop 10 2)))
  (is (= 0 (integer-power-with-loop 0 1)))
  (is (= 0 (integer-power-with-loop 0 10))))

(def test test/integer-power-with-loop/partial-eval ()
  (is (equal (partial-eval '(integer-power-with-loop base 4) :layer 'integer-power-with-loop)
             '(* base (* base (* base (* base 1)))))))

;;;;;;
;;; match-simple-regexp-with-recursion

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

(def layer match-simple-regexp-with-recursion ()
  ())

(def layered-method eval-function-call? :in match-simple-regexp-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) nil)))

(def layered-method inline-function-call? :in match-simple-regexp-with-recursion ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(match-simple-regexp-with-recursion))))

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

(def test test/match-simple-regexp-with-recursion/partial-eval ()
  (is (equal (partial-eval '(match-simple-regexp-with-recursion "a(ab)*b" text) :layer 'match-simple-regexp-with-recursion)
             nil)))

;;;;;;
;;; standard-class-without-slots

(def class standard-class-without-slots ()
  ())

(def layer standard-class-without-slots/layer ()
  ())

(def layered-method eval-function-call? :in standard-class-without-slots/layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) nil)))

(def layered-method inline-function-call? :in standard-class-without-slots/layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(standard-class-without-slots))))

(def test test/standard-class-without-slots/partial-eval ()
  (is (equal (partial-eval '(make-instance initialize-instance allocate-instance) :layer 'standard-class-without-slots/layer)
             nil)))

;;;;;;
;;; standard-class-with-slots

(def class* standard-class-with-slots ()
  ((foo 1)
   (bar 2)))

(def layer standard-class-with-slots/layer ()
  ())

(def layered-method eval-function-call? :in standard-class-with-slots/layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) nil)))

(def layered-method inline-function-call? :in standard-class-with-slots/layer ((ast free-application-form))
  (or (call-next-method)
      (member (operator-of ast) '(make-instance initialize-instance allocate-instance))))

(def test test/standard-class-with-slots/partial-eval ()
  (is (equal (partial-eval '(make-instance 'standard-class-with-slots) :layer 'standard-class-with-slots/layerp)
             nil)))
