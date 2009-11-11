;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; Common Lisp special forms
;;;
;;; TODO: catch load-time-value eval-when locally symbol-macrolet macrolet function multiple-value-call the throw unwind-protect labels progv

(def suite* (test/special-form :in test))

;;;;;;
;;; quote

(def parial-eval-test test/special-form/quote ()
  (quote 1) -> 1
  (quote (1 2)) -> '(1 2))

;;;;;;
;;; if

(def parial-eval-test test/special-form/if ()
  (if t 1 2) -> 1
  (if nil 1 2) -> 2
  (if t c 1) -> c
  (if nil 1 c) -> c
  (if c 1 2) -> (if c 1 2)
  (if (progn c) (progn 1) (progn 2)) -> (if c 1 2)
  (if (setq x t) 1 2) -> (if (setq x t) 1 2)
  (if (error t) 1 2) -> (if (error t) 1 2))

;;;;;;
;;; progn

(def parial-eval-test test/special-form/progn ()
  (progn) -> nil
  (progn 1) -> 1
  (progn 1 2) -> 2
  (progn 1 (print 1) 2 3) -> (progn (print 1) 3)
  (progn
    (if (<= repeat 0)
        (go :end)
        (setq repeat (- repeat 1)))
    (if a
        (print a))) -> (if a (print a)))

;;;;;;
;;; multiple-value-prog1

(def parial-eval-test test/special-form/multiple-value-prog1 ()
  (multiple-value-prog1 1) -> 1
  (multiple-value-prog1 1 2 3) -> 1)

;;;;;;
;;; block

(def parial-eval-test test/special-form/block ()
  (block nil) -> nil
  (block nil 1) -> 1
  (block nil 1 2) -> 2
  (block nil (print 1)) -> (print 1)
  (block nil (print 1) 2) -> (progn (print 1) 2))

;;;;;;
;;; return-from

(def parial-eval-test test/special-form/return-from ()
  (block nil (return-from nil 1)) -> 1
  (block nil 1 (return-from nil 2)) -> 2
  (block nil 1 (return-from nil 2) (print 3)) -> 2
  (block nil 1 (return-from nil 2) (error 3)) -> 2
  (block nil 1 (print 2) (return-from nil 3)) -> (progn (print 2) 3)
  (block outer 1
         (block inner 2 (return-from outer 3) (error 4))
         (error 5)) -> 3)

;;;;;;
;;; tagbody

(def parial-eval-test test/special-form/tagbody ()
  (tagbody) -> nil
  (tagbody :begin) -> nil
  (tagbody (print 1)) -> (progn (print 1) nil))

;;;;;;
;;; go

(def parial-eval-test test/special-form/go/simple ()
  (tagbody
     (go :end)
   :end) -> nil
  (tagbody
     (go :middle)
   :begin
     (go :begin)
   :middle
     (go :end)
   :end) -> nil
  (let ((repeat 3))
    (tagbody
     :begin
       (if (= repeat 0)
           (go :end)
           (setq repeat (- repeat 1)))
       (go :begin)
     :end)) -> nil
  (let ((list '(1)))
    (tagbody
     :begin
       (if (null list)
           nil
           (progn
             (setq list (cdr list))
             (go :begin))))) -> nil)

(def test test/special-form/go/complex ()
  (bind ((infinte-loop '(tagbody
                         :begin
                         (go :begin))))
    (is (equal infinte-loop (partial-eval infinte-loop))))
  (bind ((not-unrollable-loop '(tagbody
                                :begin
                                (if a
                                    (go :end)
                                    (go :begin))
                                :end
                                nil)))
    (is (equal not-unrollable-loop (partial-eval not-unrollable-loop))))
  (bind ((not-unrollable-loop '(tagbody
                                :begin
                                (if (<= repeat 0)
                                    (go :end)
                                    (setq repeat (- repeat 1)))
                                (if a
                                    (print a))
                                (go :begin)
                                :end)))
    (is (equal not-unrollable-loop (partial-eval not-unrollable-loop)))))

;;;;;;
;;; let

(def parial-eval-test test/special-form/let ()
  (let ()) -> nil
  (let () 1) -> 1
  (let () 1 2) -> 2
  (let () a) -> a
  (let () 1 a) -> a
  (let ((a 1)) a) -> 1
  (let ((a b)) a) -> b)

;;;;;;
;;; let*

(def parial-eval-test test/special-form/let* ()
  (let* ()) -> nil
  (let* () 1) -> 1
  (let* () 1 2) -> 2
  (let* () a) -> a
  (let* () 1 a) -> a
  (let* ((a 1)) a) -> 1
  (let* ((a b)) a) -> b)

;;;;;;
;;; setq

(def parial-eval-test test/special-form/setq ()
  (let ((x nil)) (setq x 1)) -> 1
  (setq x 1) -> (setq x 1))

;;;;;;
;;; flet

(def parial-eval-test test/special-form/flet ()
  (flet ((foo () 1))
    (funcall #'foo)) -> 1)

;;;;;;
;;; complex

(def parial-eval-test test/special-form/combined/simple ()
  (block nil
    (let ((result 1))
      (tagbody
         (return-from nil result)))) -> 1
  (block nil
    (tagbody
     :begin
       (progn
         (print 1)
         (return-from nil 2))
       (go :begin))) -> (progn (print 1) 2)
  (block nil
    (let ((repeat 3))
      (tagbody
       :begin
         (if (<= repeat 0)
             (go :end)
             (setq repeat (- repeat 1)))
         (if a
             (return-from nil (print a)))
         (go :begin)
       :end))) -> (block nil
                    (progn
                      (if a
                          (return (print a)))
                      (if a
                          (return (print a)))
                      (if a
                          (return (print a)))
                      nil)))

(def test test/special-form/combined/complex ()
  (bind ((undecidable-non-local-exit '(block nil
                                       (let* ((r 1))
                                         (tagbody
                                          :begin
                                            (if c
                                                (go :end)
                                                (go :begin))
                                          :end
                                            (return r))))))
    (is (equal (partial-eval undecidable-non-local-exit)
               undecidable-non-local-exit))))
