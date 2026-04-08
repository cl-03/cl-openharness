;;; test-messages.lisp - Tests for messages.lisp
;;;
;;; This file contains tests for the message structures.

(in-package #:cl-openharness/test)

(defun test-text-block ()
  "Test text block creation and access."
  (let ((block (make-text-block "Hello, World!")))
    (assert (text-block-p block))
    (assert (string= (text-block-text block) "Hello, World!"))
    (assert (string= (text-block-type block) "text"))
    :pass))

(defun test-tool-use-block ()
  "Test tool use block creation and access."
  (let ((input (make-hash-table :test 'equal))
        (block nil))
    (setf (gethash "command" input) "ls -la")
    (setf block (make-tool-use-block "Bash" input))

    (assert (tool-use-block-p block))
    (assert (string= (tool-use-block-name block) "Bash"))
    (assert (string= (tool-use-block-type block) "tool_use"))
    (assert (string= (gethash "command" (tool-use-block-input block)) "ls -la"))
    (assert (plusp (length (tool-use-block-id block))))
    :pass))

(defun test-tool-result-block ()
  "Test tool result block creation and access."
  (let ((block (make-tool-result-block "toolu_123" "output" t)))
    (assert (tool-result-block-p block))
    (assert (string= (tool-result-block-tool-use-id block) "toolu_123"))
    (assert (string= (tool-result-block-content block) "output"))
    (assert (tool-result-block-is-error block))
    (assert (string= (tool-result-block-type block) "tool_result"))
    :pass))

(defun test-conversation-message-from-user-text ()
  "Test creating a user message from text."
  (let ((msg (conversation-message-from-user-text "Hello!")))
    (assert (conversation-message-p msg))
    (assert (string= (conversation-message-role msg) "user"))
    (assert (= (length (conversation-message-content msg)) 1))
    (assert (text-block-p (car (conversation-message-content msg))))
    (assert (string= (conversation-message-text msg) "Hello!"))
    :pass))

(defun test-conversation-message-tool-uses ()
  "Test extracting tool uses from a message."
  (let* ((input (make-hash-table :test 'equal))
         (tool-use (make-tool-use-block "Bash" input))
         (text (make-text-block "Running command"))
         (msg (make-conversation-message "assistant" (list tool-use text))))
    (assert (= (length (conversation-message-tool-uses msg)) 1))
    (assert (string= (tool-use-block-name (car (conversation-message-tool-uses msg))) "Bash"))
    :pass))

(defun test-message-to-api-param ()
  "Test converting message to API params."
  (let ((msg (conversation-message-from-user-text "Test")))
    (let ((params (message-to-api-param msg)))
      (assert (alist-p params))
      (assert (string= (cdr (assoc "role" params :test 'string=)) "user"))
      (assert (listp (cdr (assoc "content" params :test 'string=))))
      :pass)))

(defun test-has-pending-continuation-p ()
  "Test pending continuation detection."
  ;; Empty messages
  (assert (not (has-pending-continuation-p nil)))

  ;; Single user message
  (assert (not (has-pending-continuation-p
                (list (conversation-message-from-user-text "Hello")))))

  ;; Assistant + User with tool result
  (let* ((tool-use (make-tool-use-block "Bash" (make-hash-table :test 'equal)))
         (tool-result (make-tool-result-block (tool-use-block-id tool-use) "output"))
         (assistant-msg (make-conversation-message "assistant" (list tool-use)))
         (user-msg (make-conversation-message "user" (list tool-result))))
    (assert (has-pending-continuation-p (list assistant-msg user-msg))))
  :pass))

(defun run-message-tests ()
  "Run all message tests."
  (let ((passed 0)
        (failed 0))
    (dolist (test '(test-text-block
                    test-tool-use-block
                    test-tool-result-block
                    test-conversation-message-from-user-text
                    test-conversation-message-tool-uses
                    test-message-to-api-param
                    test-has-pending-continuation-p))
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
