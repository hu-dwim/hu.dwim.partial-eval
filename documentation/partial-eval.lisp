;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.partial-eval.documentation)

(def project :hu.dwim.partial-eval :path (system-pathname :hu.dwim.partial-eval))

(def book user-guide (:title "User guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "TODO"))
  (chapter (:title "Supported Common Lisp Implementations")
    (paragraph ()
      "SBCL"))
  (chapter (:title "Supported Operating Systems")
    (paragraph ()
      "Linux"))
  (chapter (:title "Tutorial")
    (paragraph ()
      "TODO")))

#|
Adventures in the Land of Partial Evaluation
First steps
author: Levente Mészáros
reviewer: Attila Lendva, Tamás Borbély
;-)

Definition

this is my first blog post in a series of posts about partial
evaluation in common lisp. the idea of partial evaluation is a well
known optimization technique in computer science, unfortunately it is
less often used in practice. in common lisp a partial evaluator is
simply a function that takes as input an expression (possibly with
free variables) and a couple of assumptions about the expression's
environment. then it produces a specialized version of the input
expression that is optimized with respect to the given assumptions.
the output expression must behave exactly the same way for all
possible environments as the input expression would do as long as the
assumptions are kept. in common lisp this means the resulting
expression must have the same side effects in the same order, the same
return value and the same non local transfer of controls (both in and
out). these requirements make partial evaluation pretty difficult in
general.

Assumptions

the simplest class of assumptions speak about one of the expression's
free variables. the variable's value can be a constant number, a
constant string, a particular object, etc. other assumptions speak
about the type of one such free variable. for example the value can be
an arbitrary integer, a string, or an instance of a particular class.
some more complicated examples use arbitrary predicates such as the
value is a negative odd integer, a prime number, a fixed length string
of alphanumeric characters, a subclass of a particular class, an open
character file with utf-8 encoding, etc. there may be even more
complex assumptions that specify constraints on two or more free
variables at once

another class of assumptions speak about the expected behavior of one
of the functions that is used during the evaluation of the expression.
the function may be specified to have no side effects, not to return
freshly allocated objects, be inlined, be evaluated for constant
arguments at partial evaluation time, etc.

another class of assumptions speak about particular expressions such
as ifs, loops or recursion points that occur during the evaluation of
the input expression. one of an if's branch may be cut, a loop may be
forced to be unrolled, a recursion may be inlined, etc.

Implementation

this partial evaluator is basically a special common lisp interpreter.
the main difference is that the values of variables and function
arguments can not only be constants, but arbitrary expressions. the
partial evaluator function takes a lisp form and builds up an internal
abstract syntax tree representation using a walker. the actual partial
evaluation is done on that representation recursively resulting in
another abstract syntax tree. at the end the tree is unwalked to
produce the output expression.

constant expressions are left unchanged. individual common lisp
special forms are partial evaluated according to the language rules.
if expressions are interpreted on both branches unless it can be
proved that which branch will be always taken. to avoid exponential
growth if expressions can be cut off and left untouched by customizing
the partial evaluator or by the default guard. in common lisp loops
are implemented using the special forms tagbody and go. loop unrolling
is done by following go expressions and continuing partial evaluation
at the referred label. to avoid infinite looping the default guard
stops partial evaluation above a certain threshold and leaves the loop
in the output expression. this does not mean that the forms within the
tagbody are not partial evaluated. recursive function calls are also
guarded to avoid infinite recursion.

the partial evaluator is a bunch of layered generic functions. this
allows easy customization at various points in the algorithm while
keeping other parts intact.

Special form examples

*** copy some examples from the test suite

Ok, most mar tenyleg keso van, szoval ez osszecsapott (akar a korabbiak :)
majd be is csekkolom a partial-eval libbe a documentation ala

Draft
Adventures in the Land of Partial Evaluation
Functions

Idea

when partially evaluating the input expression there might be a call
to another function. the interesting question may be asked whether is
it possible to partially evaluate the body of the function in such a
call?

Implementation

for each called function the partial evaluator needs to get its
source. fortunately the common lisp implementation provides source
locations that can be used to lazily read the source for a given
function. when the partial evaluator finds a function call it can do
one of three things, either leave the call intact, or call the
function immediately if all arguments are constants, or inline the
function body and continue partial evaluation within.

Example
string-compare
show both recursive and iterative approach
show results of partial-evaluation

list-append
etc

integer-power
etc

match-regexp
etc

Draft
Adventures in the Land of Partial Evaluation
Generic Functions

Idea

the common lisp object system (CLOS) provides object oriented
programming for the common lisp programmer. calling a CLOS generic
function, despite the hard work compilers do in the non trivial case,
has significant overhead compared to calling a simple function. the
interesting question may be asked whether is it possible to partially
evaluate a generic function? the answer is obviously yes, but as usual
the devil is in the details. calling a generic function involves
calling other generic functions according to the meta object protocol
(MOP).

Implementation

for each generic function the partial evaluator builds a lambda form
that implements the discrimination and calls the generic method bodies
according to the CLOS rules. this is obviously already present in the
common lisp implementation in some form, but unfortunately it turns
out to be not so trivial to get what we want. there's no problem with
calling other functions, even generic functions from the generated
lambda form as long as it does not refer to the original generic
function. once the partial evaluator has the lambda form, the original
generic function call becomes a simple function call that it can
already partially evaluate.

Example
print-dispatch-information
show original source
show lambda form built for the generic function
show partial evaluation for constant arguments
show partial evaluation for constant argument types

Draft
Adventures in the Land of Partial Evaluation
Meta object protocol

Idea

the common lisp object system (CLOS) has a quite flexible, but also
quite complicated meta object protocol (MOP). one of the most often
used protocol is the class instance creation. most common lisp
implementations do a fairly good job when optimizing class instance
creation as long as certain constraints are kept when defining various
customizations on the involved generic functions and when calling the
protocol. the interesting question may be asked whether is it possible
to partially evaluate the class instance creation protocol? more
specifically is it possible to do so with respect to a particular
class and initialization arguments and achieve similar performance
when comparing to the hand made optimizations provided by current lisp
compilers.

Implementation
the class instance creation protocol uses the following generic
functions: make-instance, allocate-instance, initialize-instance,
shared-initialize, (setf slot-value-using-class) and some others. to
achieve the performance goal we must be able to inline and partially
evaluate all these generic functions.

show customizations
show functions evaluated at partial evaluation time
show inlined functions
show special variables evaluated at partial evaluation time

Example
a standard class without slots
show calling make-instance without initialization arguments
show performance compared to the compiler's default optimization

a standard class with slots
calling make-instance with constant initialization arguments
show performance compared to the compiler's default optimization

|#