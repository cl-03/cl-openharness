;;; package.lisp - Test package for cl-openharness
;;;
;;; This file defines the test package for cl-openharness tests.

(defpackage #:cl-openharness/test
  (:nicknames #:oh-test)
  (:use #:cl #:cl-openharness)
  (:export #:run-all-tests))

(in-package #:cl-openharness/test)
