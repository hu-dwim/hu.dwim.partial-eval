;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.partial-eval.documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.partial-eval.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "adventures" :depends-on ("package"))
                             (:file "install-guide" :depends-on ("package"))
                             (:file "package")
                             (:file "project" :depends-on ("package"))
                             (:file "user-guide" :depends-on ("package"))))))
