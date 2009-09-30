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
  (is (equal '(if c 1 2) (partial-eval '(if c 1 2))))
  (is (equal '(if c 1 2) (partial-eval '(if (progn c) (progn 1) (progn 2))))))

;;;;;;
;;; progn

(def test test/special-form/progn ()
  (is (equal nil (partial-eval '(progn))))
  (is (equal 1 (partial-eval '(progn 1))))
  (is (equal 2 (partial-eval '(progn 1 2)))))

;;;;;;
;;; block

(def test test/special-form/block ()
  (is (equal nil (partial-eval '(block nil))))
  (is (equal 1 (partial-eval '(block nil 1))))
  (is (equal 2 (partial-eval '(block nil 1 2)))))

;;;;;;
;;; return-from

(def test test/special-form/return-from ()
  (is (equal t (partial-eval '(block nil (return-from nil t)))))
  (is (equal t (partial-eval '(block nil (return-from nil t) (print 1)))))
  (is (equal t (partial-eval '(block nil (return-from nil t) (error 1))))))

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
                                 :end)))))

;;;;;;
;;; let

(def test test/special-form/let ()
  (is (equal nil (partial-eval '(let ()))))
  (is (equal 1 (partial-eval '(let () 1))))
  (is (equal 2 (partial-eval '(let () 1 2)))))

;;;;;;
;;; setq

(def test test/special-form/setq ()
  (is (equal t (partial-eval '(let ((x nil)) (setq x t)))))
  (is (equal '(setq x t) (partial-eval '(setq x t)))))
