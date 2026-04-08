;;; test-tools.lisp - Tests for tools.lisp
;;;
;;; This file contains tests for the tool registry and execution.

(in-package #:cl-openharness/test)

(defun test-tool-registry-create ()
  "Test tool registry creation."
  (let ((registry (make-tool-registry)))
    (assert (tool-registry-p registry))
    (assert (= (length (tool-registry-list-tools registry)) 0))
    :pass))

(defun test-tool-registry-register ()
  "Test tool registration."
  (let ((registry (make-tool-registry))
        (tool (make-function-tool "test-tool" "A test tool"
                                  (lambda (args ctx)
                                    (declare (ignore args ctx))
                                    (make-tool-result "ok")))))
    (tool-registry-register registry tool)
    (assert (= (length (tool-registry-list-tools registry)) 1))
    (assert (eq (tool-registry-get registry "test-tool") tool))
    :pass))

(defun test-tool-registry-has-p ()
  "Test tool existence check."
  (let ((registry (make-tool-registry))
        (tool (make-function-tool "existing-tool" "Test"
                                  (lambda (args ctx)
                                    (make-tool-result "ok")))))
    (tool-registry-register registry tool)
    (assert (tool-registry-has-p registry "existing-tool"))
    (assert (not (tool-registry-has-p registry "nonexistent-tool")))
    :pass))

(defun test-tool-registry-to-api-schema ()
  "Test tool schema generation."
  (let ((registry (make-tool-registry))
        (tool (make-function-tool "test-tool" "A test tool"
                                  (lambda (args ctx)
                                    (make-tool-result "ok"))
                                  :input-schema '(("type" . "object")))))
    (tool-registry-register registry tool)
    (let ((schemas (tool-registry-to-api-schema registry)))
      (assert (= (length schemas) 1))
      (assert (string= (cdr (assoc "name" (car schemas) :test 'string=)) "test-tool"))
      (assert (string= (cdr (assoc "description" (car schemas) :test 'string=)) "A test tool"))
      :pass)))

(defun test-tool-execution ()
  "Test tool execution."
  (let ((tool (make-function-tool "echo-tool" "Echoes input"
                                  (lambda (args ctx)
                                    (let ((text (gethash "text" args "")))
                                      (make-tool-result (format nil "Echo: ~a" text))))
                                  :input-schema '(("type" . "object")
                                                  ("properties" . (("text" . (("type" . "string"))))))))
        (args (make-hash-table :test 'equal))
        (context (make-tool-execution-context "/tmp")))
    (setf (gethash "text" args) "Hello")
    (let ((result (tool-execute tool args context)))
      (assert (tool-result-p result))
      (assert (string= (tool-result-output result) "Echo: Hello"))
      (assert (not (tool-result-is-error result))))
    :pass))

(defun test-tool-result ()
  "Test tool result creation."
  (let ((result (make-tool-result "output" :is-error t :metadata '(("key" . "value")))))
    (assert (string= (tool-result-output result) "output"))
    (assert (tool-result-is-error result))
    (assert (string= (gethash "key" (tool-result-metadata result)) "value"))
    :pass))

(defun test-tool-execution-context ()
  "Test tool execution context."
  (let ((ctx (make-tool-execution-context "/test/path" '(("user" . "test")))))
    (assert (string= (tool-execution-context-cwd ctx) "/test/path"))
    (assert (string= (gethash "user" (tool-execution-context-metadata ctx)) "test"))
    :pass))

(defun test-builtin-tools ()
  "Test builtin tool registration."
  (let ((registry (make-tool-registry)))
    (register-builtin-tools registry)
    (assert (tool-registry-has-p registry "Bash"))
    (assert (tool-registry-has-p registry "Read"))
    (assert (tool-registry-has-p registry "Write"))
    (assert (tool-registry-has-p registry "Edit"))
    (assert (tool-registry-has-p registry "Glob"))
    (assert (tool-registry-has-p registry "Grep"))
    :pass))

(defun run-tools-tests ()
  "Run all tools tests."
  (let ((passed 0)
        (failed 0))
    (dolist (test '(test-tool-registry-create
                    test-tool-registry-register
                    test-tool-registry-has-p
                    test-tool-registry-to-api-schema
                    test-tool-execution
                    test-tool-result
                    test-tool-execution-context
                    test-builtin-tools))
      (format t "Running ~a... " test)
      (handler-case
          (let ((result (funcall test)))
            (if (eq result :pass)
                (progn
                  (format t "~a" :pass)
                  (incf passed))
                (progn
                  (format t "~a - unexpected result: ~s" :fail result)
                  (incf failed))))
        (error (e)
          (format t "~a - ~a" :fail e)
          (incf failed)))
      (terpri))
    (format t "~%~%Results: ~a passed, ~a failed~%" passed failed)
    (values passed failed)))
