;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.test)

;;;;;;
;;; Common Lisp special forms

(def suite* (test/special-form :in test))

;;;;;;
;;; if

(def test test/special-form/if ()
  (is (equal 1 (partial-eval '(if t 1 2))))
  (is (equal 2 (partial-eval '(if nil 1 2))))
  (is (equal 'c (partial-eval '(if t c 1))))
  (is (equal 'c (partial-eval '(if nil 1 c))))
  (is (equal '(if c 1 2) (partial-eval '(if c 1 2))))
  (is (equal '(if c 1 2) (partial-eval '(if (progn c) (progn 1) (progn 2)))))
  (is (equal '(if (setq x t) 1 2) (partial-eval '(if (setq x t) 1 2))))
  (is (equal '(if (error t) 1 2) (partial-eval '(if (error t) 1 2)))))

;;;;;;
;;; progn

(def test test/special-form/progn ()
  (is (equal nil (partial-eval '(progn))))
  (is (equal 1 (partial-eval '(progn 1))))
  (is (equal 2 (partial-eval '(progn 1 2))))
  (is (equal '(progn (print 1) 3) (partial-eval '(progn 1 (print 1) 2 3)))))

;;;;;;
;;; block

(def test test/special-form/block ()
  (is (equal nil (partial-eval '(block nil))))
  (is (equal 1 (partial-eval '(block nil 1))))
  (is (equal 2 (partial-eval '(block nil 1 2)))))

;;;;;;
;;; return-from

(def test test/special-form/return-from ()
  (is (equal 2 (partial-eval '(block nil 1 (return-from nil 2)))))
  (is (equal 2 (partial-eval '(block nil 1 (return-from nil 2) (print 3)))))
  (is (equal 2 (partial-eval '(block nil 1 (return-from nil 2) (error 3)))))
  (is (equal 3 (partial-eval '(block outer 1
                               (block inner 2 (return-from outer 3) (error 4))
                               (error 5))))))

;;;;;;
;;; tagbody

(def test test/special-form/tagbody ()
  (is (equal nil (partial-eval '(tagbody))))
  (is (equal nil (partial-eval '(tagbody 1))))
  (is (equal nil (partial-eval '(tagbody 1 2)))))

;;;;;;
;;; go

(def test test/special-form/go ()
  (is (equal nil (partial-eval '(tagbody
                                 (go :end)
                                 :end))))
  (is (equal nil (partial-eval '(tagbody
                                 (go :middle)
                                 :begin
                                 (go :begin)
                                 :middle
                                 (go :end)
                                 :end))))
  (bind ((infinte-loop '(tagbody
                         :begin
                         (go :begin))))
    (is (equal infinte-loop (partial-eval infinte-loop))))
  (bind ((undecidable-loop '(tagbody
                             :begin
                             (if a
                                 (go :end)
                                 (go :begin))
                             :end
                             nil)))
    (is (equal undecidable-loop (partial-eval undecidable-loop))))
  (is (equal nil (partial-eval '(let ((repeat 3))
                                 (tagbody
                                  :begin
                                    (if (<= repeat 0)
                                        (go :end)
                                        (setq repeat (- repeat 1)))
                                    (go :begin)
                                  :end))))))

;;;;;;
;;; let

(def test test/special-form/let ()
  (is (equal nil (partial-eval '(let ()))))
  (is (equal 1 (partial-eval '(let () 1))))
  (is (equal 2 (partial-eval '(let () 1 2)))))

;;;;;;
;;; setq

(def test test/special-form/setq ()
  (is (equal 1 (partial-eval '(let ((x nil)) (setq x 1)))))
  (is (equal '(setq x 1) (partial-eval '(setq x 1)))))

;;;;;;
;;; complex

(def test test/special-form/complex ()
  (is (equal 1 (partial-eval '(block nil
                               (let ((result 1))
                                 (tagbody
                                    (return-from nil result)))))))
  (bind ((undecidable-non-local-exit '(block nil
                                       (let* ((r 1))
                                         (tagbody
                                          :begin
                                            (if c
                                                (go :end)
                                                (go :begin))
                                          :end
                                            (return-from nil r))))))
    (partial-eval undecidable-non-local-exit)))
