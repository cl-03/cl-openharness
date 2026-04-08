;;; basic-tests.lisp - Simple tests for cl-openharness
;;;
;;; Basic functionality tests without external dependencies

(in-package #:cl-openharness)

(defun run-basic-tests ()
  "Run basic functionality tests and return summary."
  (format t "~%========================================~%")
  (format t "cl-openharness Basic Tests~%")
  (format t "========================================~%~%")

  (let ((passed 0) (failed 0))
    (flet ((test (name fn)
             (format t "Test: ~a ... " name)
             (handler-case
                 (progn
                   (funcall fn)
                   (format t "PASS~%")
                   (incf passed))
               (error (e)
                 (format t "FAIL: ~a~%" e)
                 (incf failed)))))

      ;; Test 1: Text block creation
      (test "text-block creation"
        (lambda ()
          (let ((block (make-text-block :text "Hello")))
            (assert (text-block-p block))
            (assert (string= (text-block-text block) "Hello")))))

      ;; Test 2: Tool use block creation
      (test "tool-use-block creation"
        (lambda ()
          (let ((block (make-tool-use-block :name "Bash" :id "test-1")))
            (assert (tool-use-block-p block))
            (assert (string= (tool-use-block-name block) "Bash")))))

      ;; Test 3: Tool result block creation
      (test "tool-result-block creation"
        (lambda ()
          (let ((block (make-tool-result-block :tool-use-id "test-1" :content "result" :is-error nil)))
            (assert (tool-result-block-p block))
            (assert (string= (tool-result-block-content block) "result")))))

      ;; Test 4: Conversation message creation
      (test "conversation-message creation"
        (lambda ()
          (let ((msg (make-conversation-message :role "user" :content nil)))
            (assert (conversation-message-p msg))
            (assert (string= (conversation-message-role msg) "user")))))

      ;; Test 5: User message from text
      (test "conversation-message-from-user-text"
        (lambda ()
          (let ((msg (conversation-message-from-user-text "Hello world")))
            (assert (conversation-message-p msg))
            (assert (string= (conversation-message-role msg) "user")))))

      ;; Test 6: Tool registry
      (test "tool-registry creation"
        (lambda ()
          (let ((registry (make-tool-registry)))
            (assert (tool-registry-p registry)))))

      ;; Test 7: Permission settings
      (test "permission-settings creation"
        (lambda ()
          (let ((settings (make-permission-settings)))
            (assert (permission-settings-p settings)))))

      ;; Test 8: Permission checker
      (test "permission-checker creation"
        (lambda ()
          (let ((checker (create-permission-checker (make-permission-settings))))
            (assert (permission-checker-p checker)))))

      ;; Test 9: API client
      (test "api-client creation"
        (lambda ()
          (let ((client (make-api-client)))
            (assert (api-client-p client)))))

      ;; Test 10: Runtime bundle
      (test "runtime-bundle creation"
        (lambda ()
          (let ((bundle (make-runtime-bundle :model "test-model")))
            (assert (runtime-bundle-p bundle))
            (assert (string= (runtime-bundle-model bundle) "test-model")))))

      ;; Test 11: Harness creation
      (test "harness creation"
        (lambda ()
          (let ((h (make-harness)))
            (assert (harness-p h)))))

      ;; Test 12: Session info
      (test "session-info creation"
        (lambda ()
          (let ((info (make-session-info :id "test-session")))
            (assert (session-info-p info))
            (assert (string= (session-info-id info) "test-session")))))

      ;; Test 13: Built-in tools registration
      (test "built-in tools registration"
        (lambda ()
          (let ((registry (make-tool-registry)))
            (register-builtin-tools registry)
            (let ((tools (tool-registry-list-tools registry)))
              (assert (> (length tools) 15))
              (format t "(~a tools registered) " (length tools))))))

      ;; Test 14: Required tools exist
      (test "required tools exist"
        (lambda ()
          (let ((registry (make-tool-registry)))
            (register-builtin-tools registry)
            (dolist (tool-name '("Bash" "Read" "Write" "Edit" "Glob" "Grep"
                                 "Delete" "Copy" "Move" "ListDirectory" "Touch"
                                 "Cat" "Head" "Tail" "Wc" "JsonParse" "HttpRequest"))
              (assert (tool-registry-has-p registry tool-name) ()
                      "Tool ~a not found" tool-name))
            (format t "(17 tools verified) "))))

      (format t "~%========================================~%")
      (format t "TOTAL: ~a passed, ~a failed~%" passed failed)
      (format t "========================================~%")

      (if (= failed 0)
          (format t "~%All tests passed!~%")
          (format t "~%~a tests failed!~%" failed))

      (values passed failed))))
