;;; run-tests.lisp - Test runner for cl-openharness
;;;
;;; This file provides the main test runner function.

(in-package #:cl-openharness/test)

(defun run-all-tests ()
  "Run all tests and return summary."
  (format t "~%========================================~%")
  (format t "cl-openharness Test Suite~%")
  (format t "========================================~%~%")

  (let ((total-passed 0)
        (total-failed 0))

    ;; Message tests
    (format t "--- Message Tests ---~%")
    (multiple-value-bind (passed failed)
        (run-message-tests)
      (incf total-passed passed)
      (incf total-failed failed))

    (format t "~%")

    ;; Tools tests
    (format t "--- Tools Tests ---~%")
    (multiple-value-bind (passed failed)
        (run-tools-tests)
      (incf total-passed passed)
      (incf total-failed failed))

    (format t "~%")

    ;; Permissions tests
    (format t "--- Permissions Tests ---~%")
    (multiple-value-bind (passed failed)
        (run-permissions-tests)
      (incf total-passed passed)
      (incf total-failed failed))

    (format t "~%========================================~%")
    (format t "TOTAL: ~a passed, ~a failed~%" total-passed total-failed)
    (format t "========================================~%")

    (values total-passed total-failed)))
