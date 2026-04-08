;;; examples.lisp - Usage examples for cl-openharness
;;;
;;; This file demonstrates how to use cl-openharness.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Example 1: Simple Single Prompt
;;; ============================================================================

(defun example-simple-prompt ()
  "Example: Run a single prompt through the harness."
  ;; Note: Replace with your actual API key
  (let ((api-key "your-api-key-here"))
    (when (string= api-key "your-api-key-here")
      (format t "Please set your API key~%")
      (return-from example-simple-prompt nil))

    (let ((response (harness-run-single
                     "What is the capital of France?"
                     :api-key api-key
                     :model "claude-sonnet-4-6")))
      (format t "~%Response: ~a~%" response))))

;;; ============================================================================
;;; Example 2: Interactive Session
;;; ============================================================================

(defun example-interactive-session ()
  "Example: Run an interactive session with multiple turns."
  (with-harness (h :api-key "your-api-key-here"
                   :model "claude-sonnet-4-6"
                   :max-turns 10
                   :permission-mode "default")
    ;; First prompt
    (format t "~%User: What is 2 + 2?~%")
    (multiple-value-bind (events usage)
        (harness-run h "What is 2 + 2?")
      (declare (ignore events))
      (format t "Harness: [response]~%")
      (format t "Usage: ~a input tokens, ~a output tokens~%"
              (usage-snapshot-input-tokens usage)
              (usage-snapshot-output-tokens usage)))

    ;; Second prompt
    (format t "~%User: Now multiply that by 3~%")
    (multiple-value-bind (events usage)
        (harness-run h "Now multiply that by 3")
      (declare (ignore events))
      (format t "Harness: [response]~%")
      (format t "Total usage: ~a input tokens, ~a output tokens~%"
              (usage-snapshot-input-tokens (harness-usage h))
              (usage-snapshot-output-tokens (harness-usage h))))))

;;; ============================================================================
;;; Example 3: Custom Tool
;;; ============================================================================

(defun example-custom-tool ()
  "Example: Register and use a custom tool."
  ;; Create a custom tool
  (let ((calculator-tool (make-function-tool
                          "calculate"
                          "Perform mathematical calculations"
                          (lambda (arguments context)
                            (let ((expression (gethash "expression" arguments "")))
                              (handler-case
                                  (let ((result 0))  ; Replace with actual eval
                                    (make-tool-result (format nil "Result: ~a" result)))
                                (error (e)
                                  (make-tool-result (format nil "Error: ~a" e) :is-error t)))))
                          :input-schema '(("type" . "object")
                                          ("properties" . (("expression" . (("type" . "string")
                                                                             ("description" . "Mathematical expression to evaluate")))))
                                          ("required" . ("expression"))))))

    ;; Create harness with custom tool
    (with-harness (h :api-key "your-api-key-here"
                     :tools (list calculator-tool))
      (harness-run h "Calculate 5 + 3"))))

;;; ============================================================================
;;; Example 4: Permission Rules
;;; ============================================================================

(defun example-permission-rules ()
  "Example: Configure path-based permission rules."
  (with-harness (h :api-key "your-api-key-here"
                   :permission-mode "default"
                   :path-rules (list (make-path-rule "/etc/*" nil)  ; Deny /etc access
                                    (make-path-rule "~/projects/*" t))  ; Allow projects
                   :denied-commands (list "rm -rf *"))  ; Deny dangerous commands
      (harness-run h "List files in /etc")))

;;; ============================================================================
;;; Example 5: Hooks
;;; ============================================================================

(defun example-hooks ()
  "Example: Add PreToolUse and PostToolUse hooks."
  (let ((hook-registry (make-hook-registry)))
    ;; Add a command hook that logs tool usage
    (hook-registry-add hook-registry
      (make-command-hook +hook-event-pre-tool-use+
                        "echo 'Tool called: $ARGUMENTS' >> /tmp/tool-log.txt"
                        :matcher "*"))

    ;; Add a PostToolUse hook
    (hook-registry-add hook-registry
      (make-command-hook +hook-event-post-tool-use+
                        "echo 'Tool completed: $ARGUMENTS' >> /tmp/tool-log.txt"
                        :matcher "*"))

    (with-harness (h :api-key "your-api-key-here"
                     :hooks (list (hook-registry-get hook-registry +hook-event-pre-tool-use+)
                                 (hook-registry-get hook-registry +hook-event-post-tool-use+)))
      (harness-run h "List files in current directory"))))

;;; ============================================================================
;;; Example 6: File Operations
;;; ============================================================================

(defun example-file-operations ()
  "Example: Use built-in file operation tools."
  (with-harness (h :api-key "your-api-key-here"
                   :cwd "/tmp/cl-openharness-example")
    ;; Write a file
    (format t "~%Writing file...~%")
    (harness-run h "Write 'Hello, Common Lisp!' to /tmp/test.txt")

    ;; Read the file
    (format t "~%Reading file...~%")
    (harness-run h "Read /tmp/test.txt")

    ;; Edit the file
    (format t "~%Editing file...~%")
    (harness-run h "Replace 'Common Lisp' with 'World' in /tmp/test.txt")

    ;; Read again to see changes
    (format t "~%Reading file again...~%")
    (harness-run h "Read /tmp/test.txt")))

;;; ============================================================================
;;; Example 7: Code Search
;;; ============================================================================

(defun example-code-search ()
  "Example: Use Glob and Grep tools for code search."
  (with-harness (h :api-key "your-api-key-here"
                   :cwd "/path/to/your/project")
    ;; Find all Lisp files
    (format t "~%Finding Lisp files...~%")
    (harness-run h "Find all .lisp files in the current directory")

    ;; Search for a pattern
    (format t "~%Searching for 'defun'...~%")
    (harness-run h "Search for 'defun' in all .lisp files")))

;;; ============================================================================
;;; Example 8: Session Management
;;; ============================================================================

(defun example-session-management ()
  "Example: Manage conversation sessions."
  (let ((harness (make-harness :bundle (make-runtime-bundle
                                        :api-key "your-api-key-here"
                                        :model "claude-sonnet-4-6"))))
    ;; Get session info
    (format t "Session ID: ~a~%" (harness-session-id harness))

    ;; Run some prompts
    (harness-run harness "Hello, my name is Alice.")
    (harness-run harness "What is my name?")

    ;; Get message count
    (format t "Message count: ~a~%"
            (length (harness-messages harness)))

    ;; Clear conversation
    (harness-clear harness)
    (format t "After clear - Message count: ~a~%"
            (length (harness-messages harness)))

    ;; Stop harness
    (harness-stop harness)))

;;; ============================================================================
;;; Example 9: API Configuration
;;; ============================================================================

(defun example-api-configuration ()
  "Example: Configure different API endpoints."
  ;; Using Anthropic-compatible API (e.g., Kimi)
  (with-harness (h :api-key "your-kimi-api-key"
                   :base-url "https://api.moonshot.cn/anthropic"
                   :model "kimi-k2.5")
    (harness-run h "Hello!"))

  ;; Using OpenAI-compatible API would need a different client implementation
  ;; This is a placeholder for future extension
  )

;;; ============================================================================
;;; Example 10: Batch Processing
;;; ============================================================================

(defun example-batch-processing ()
  "Example: Process multiple prompts in batch."
  (let ((prompts '("What is the weather today?"
                   "Summarize this article..."
                   "Fix the bug in this code...")))
    (with-harness (h :api-key "your-api-key-here")
      (dolist (prompt prompts)
        (format t "~%Processing: ~a~%" prompt)
        (harness-run h prompt)
        (format t "Messages so far: ~a~%" (length (harness-messages h)))))))

;;; ============================================================================
;;; Run All Examples
;;; ============================================================================

(defun run-all-examples ()
  "Run all examples (will fail without valid API key)."
  (format t "~%Running cl-openharness examples~%")
  (format t "~%Note: Set your API key before running~%~%")

  ;; Uncomment examples to run them
  ;; (example-simple-prompt)
  ;; (example-interactive-session)
  ;; (example-custom-tool)
  ;; (example-permission-rules)
  ;; (example-hooks)
  ;; (example-file-operations)
  ;; (example-code-search)
  ;; (example-session-management)
  ;; (example-api-configuration)
  ;; (example-batch-processing)

  (format t "Examples completed.~%"))
