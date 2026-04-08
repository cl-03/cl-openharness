;;; query-engine.lisp - Query engine and agent loop
;;;
;;; This file defines the query context, query engine, and the core
;;; agent loop that powers the harness.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Query Context
;;; ============================================================================

(defstruct query-context
  "Context for running a query."
  (api-client nil :type (or null api-client))
  (tool-registry nil :type (or null tool-registry))
  (permission-checker nil :type (or null permission-checker))
  (cwd nil :type (or null pathname string))
  (model "" :type string)
  (system-prompt "" :type string)
  (max-tokens 4096 :type integer)
  (max-turns nil :type (or null integer))
  (permission-prompt nil :type t)
  (ask-user-prompt nil :type t)
  (hook-executor nil :type (or null hook-executor))
  (tool-metadata (make-hash-table :test 'equal) :type hash-table))

;;; ============================================================================
;;; Query Engine
;;; ============================================================================

(defstruct query-engine
  "Owns conversation history and the tool-aware model loop."
  (api-client nil :type (or null api-client))
  (tool-registry nil :type (or null tool-registry))
  (permission-checker nil :type (or null permission-checker))
  (cwd nil :type (or null pathname string))
  (model "" :type string)
  (system-prompt "" :type string)
  (max-tokens 4096 :type integer)
  (max-turns nil :type (or null integer))
  (permission-prompt nil :type t)
  (ask-user-prompt nil :type t)
  (hook-executor nil :type (or null hook-executor))
  (tool-metadata (make-hash-table :test 'equal) :type hash-table)
  (messages nil :type list)
  (cost-tracker nil :type cost-tracker))

(defun query-engine-total-usage (engine)
  "Return the total usage across all turns."
  (cost-tracker-total (query-engine-cost-tracker engine)))

(defun query-engine-clear (engine)
  "Clear the in-memory conversation history."
  (setf (query-engine-messages engine) nil)
  (setf (query-engine-cost-tracker engine) (make-cost-tracker))
  engine)

(defun query-engine-set-system-prompt (engine prompt)
  "Update the active system prompt for future turns."
  (setf (query-engine-system-prompt engine) prompt)
  engine)

(defun query-engine-set-model (engine model)
  "Update the active model for future turns."
  (setf (query-engine-model engine) model)
  engine)

(defun query-engine-set-api-client (engine api-client)
  "Update the active API client for future turns."
  (setf (query-engine-api-client engine) api-client)
  engine)

(defun query-engine-set-max-turns (engine max-turns)
  "Update the maximum number of agentic turns per user input."
  (setf (query-engine-max-turns engine)
        (if max-turns (max 1 max-turns) nil))
  engine)

(defun query-engine-set-permission-checker (engine checker)
  "Update the active permission checker for future turns."
  (setf (query-engine-permission-checker engine) checker)
  engine)

(defun query-engine-load-messages (engine messages)
  "Replace the in-memory conversation history."
  (setf (query-engine-messages engine) (copy-list messages))
  engine)

(defun query-engine-has-pending-continuation-p (engine)
  "Check if conversation ends with tool results awaiting follow-up."
  (has-pending-continuation-p (query-engine-messages engine)))

;;; ============================================================================
;;; Run Query
;;; ============================================================================

(defun query-engine-submit (engine prompt)
  "Append a user message and execute the query loop.
   Returns multiple values: (events usage)"
  ;; Add user message
  (push (conversation-message-from-user-text prompt)
        (query-engine-messages engine))

  ;; Build query context
  (let ((context (make-query-context
                  :api-client (query-engine-api-client engine)
                  :tool-registry (query-engine-tool-registry engine)
                  :permission-checker (query-engine-permission-checker engine)
                  :cwd (query-engine-cwd engine)
                  :model (query-engine-model engine)
                  :system-prompt (query-engine-system-prompt engine)
                  :max-tokens (query-engine-max-tokens engine)
                  :max-turns (query-engine-max-turns engine)
                  :permission-prompt (query-engine-permission-prompt engine)
                  :ask-user-prompt (query-engine-ask-user-prompt engine)
                  :hook-executor (query-engine-hook-executor engine)
                  :tool-metadata (query-engine-tool-metadata engine))))
    (run-query context (query-engine-messages engine) engine)))

(defun query-engine-continue (engine &optional (max-turns nil))
  "Continue an interrupted tool loop without appending a new user message."
  (let ((context (make-query-context
                  :api-client (query-engine-api-client engine)
                  :tool-registry (query-engine-tool-registry engine)
                  :permission-checker (query-engine-permission-checker engine)
                  :cwd (query-engine-cwd engine)
                  :model (query-engine-model engine)
                  :system-prompt (query-engine-system-prompt engine)
                  :max-tokens (query-engine-max-tokens engine)
                  :max-turns (or max-turns (query-engine-max-turns engine))
                  :permission-prompt (query-engine-permission-prompt engine)
                  :ask-user-prompt (query-engine-ask-user-prompt engine)
                  :hook-executor (query-engine-hook-executor engine)
                  :tool-metadata (query-engine-tool-metadata engine))))
    (run-query context (query-engine-messages engine) engine)))

;;; ============================================================================
;;; Query Loop
;;; ============================================================================

(defun run-query (context messages engine)
  "Run the query loop.
   Returns multiple values: (events usage)"
  (let ((turns 0)
        (events nil)
        (total-usage (make-usage-snapshot)))
    (loop
      ;; Check max turns
      (when (and (query-context-max-turns context)
                 (>= turns (query-context-max-turns context)))
        (%log :warning "Reached max turns limit (~a)" (query-context-max-turns context))
        (return-from run-query (values (nreverse events) total-usage)))

      ;; Call API
      (let ((request (make-api-request
                      :model (query-context-model context)
                      :messages messages
                      :system-prompt (query-context-system-prompt context)
                      :max-tokens (query-context-max-tokens context)
                      :tools (tool-registry-to-api-schema
                              (query-context-tool-registry context)))))
        (multiple-value-bind (event usage)
            (api-client-stream-message (query-context-api-client context) request)

          ;; Track usage
          (when usage
            (incf (usage-snapshot-input-tokens total-usage)
                  (usage-snapshot-input-tokens usage))
            (incf (usage-snapshot-output-tokens total-usage)
                  (usage-snapshot-output-tokens usage))
            (when engine
              (cost-tracker-add (query-engine-cost-tracker engine) usage)))

          ;; Collect event
          (push event events)

          ;; Check if model is done
          (if (api-message-complete-event-p event)
              (progn
                ;; Add assistant message
                (push (api-message-complete-event-message event) messages)
                (return-from run-query (values (nreverse events) total-usage)))
              ;; Process tool uses
              (progn
                (incf turns)
                (let ((tool-results (process-tool-uses
                                     event context messages)))
                  (when tool-results
                    (push tool-results messages)
                    ;; Continue loop
                    ))))))))

(defun process-tool-uses (event context messages)
  "Process tool uses from an event and return tool results message."
  (let ((tool-results nil)
        (tool-registry (query-context-tool-registry context))
        (permission-checker (query-context-permission-checker context))
        (hook-executor (query-context-hook-executor context))
        (cwd (query-context-cwd context)))

    ;; Get tool uses from the assistant message
    (let ((assistant-msg (api-message-complete-event-message event)))
      (when assistant-msg
        (dolist (tool-use (conversation-message-tool-uses assistant-msg))
          (let* ((tool-name (tool-use-block-name tool-use))
                 (tool-input (tool-use-block-input tool-use))
                 (tool (tool-registry-get tool-registry tool-name)))

            (if tool
                (progn
                  ;; Check permissions
                  (let ((is-read-only (tool-is-read-only tool tool-input))
                        (decision (permission-evaluate
                                   permission-checker tool-name
                                   :is-read-only is-read-only)))

                    (if (permission-allowed-p decision)
                        ;; Execute hook -> tool -> hook
                        (let ((result (execute-tool-with-hooks
                                       tool tool-input cwd hook-executor
                                       (query-context-api-client context))))
                          (push (make-tool-result-block
                                 :tool-use-id (tool-use-block-id tool-use)
                                 :content (tool-result-output result)
                                 :is-error (tool-result-is-error result))
                                tool-results))
                        ;; Permission denied
                        (push (make-tool-result-block
                               :tool-use-id (tool-use-block-id tool-use)
                               :content (permission-decision-reason decision)
                               :is-error t)
                              tool-results))))
                ;; Tool not found
                (push (make-tool-result-block
                       :tool-use-id (tool-use-block-id tool-use)
                       :content (format nil "Tool '~a' not found" tool-name)
                       :is-error t)
                      tool-results)))))))

    (when tool-results
      (make-conversation-message :role "user" :content (nreverse tool-results)))))

(defun execute-tool-with-hooks (tool arguments cwd hook-executor api-client)
  "Execute a tool with pre/post hooks."
  (let ((tool-name (tool-name tool))
        (result nil))

    ;; PreToolUse hooks
    (when hook-executor
      (let ((pre-result (hook-execute
                         hook-executor +hook-event-pre-tool-use+
                         (list (cons "tool_name" tool-name)
                               (cons "arguments" arguments)))))
        (when (aggregated-hook-result-blocked-p pre-result)
          (return-from execute-tool-with-hooks
            (make-tool-result
             :output (format nil "Blocked by PreToolUse hook: ~a"
                     (aggregated-hook-result-reasons pre-result))
             :is-error t)))))

    ;; Execute tool
    (handler-case
        (setf result (tool-execute tool arguments
                                   (make-tool-execution-context :cwd cwd)))
      (error (e)
        (setf result (make-tool-result :output (format nil "Error: ~a" e) :is-error t))))

    ;; PostToolUse hooks
    (when hook-executor
      (let ((post-result (hook-execute
                          hook-executor +hook-event-post-tool-use+
                          (list (cons "tool_name" tool-name)
                                (cons "result" (tool-result-output result))))))
        (when (aggregated-hook-result-blocked-p post-result)
          (%log :warning "PostToolUse hook blocked: ~a"
               (aggregated-hook-result-reasons post-result)))))

    result))

(defun aggregated-hook-result-reasons (aggregated)
  "Get reasons from all hook results."
  (mapcar #'hook-result-reason (aggregated-hook-result-results aggregated)))
