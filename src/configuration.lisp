;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(def function transform-function-definer-options (options)
  (if hu.dwim.partial-eval.system:*load-as-production-p*
      options
      (remove-from-plist options :inline :optimize)))

(def function setup-readtable ()
  (enable-sharp-boolean-syntax))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '("HU.DWIM.PARTIAL-EVAL" "HU.DWIM.PARTIAL-EVAL.TEST") 'setup-readtable)
