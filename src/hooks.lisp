;;; hooks.lisp - Hook execution engine
;;;
;;; This file defines hook events, definitions, registry, and executor
;;; for lifecycle events like PreToolUse and PostToolUse.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Hook Events
;;; ============================================================================

(defparameter +hook-event-pre-tool-use+ "PreToolUse"
  "Hook event fired before tool execution.")

(defparameter +hook-event-post-tool-use+ "PostToolUse"
  "Hook event fired after tool execution.")

(defparameter +hook-event-stop+ "Stop"
  "Hook event fired when session ends.")

;;; ============================================================================
;;; Hook Result
;;; ============================================================================

(defstruct hook-result
  "Result of a hook execution."
  (hook-type "" :type string)
  (success nil :type boolean)
  (output "" :type string)
  (blocked nil :type boolean)
  (reason "" :type string)
  (metadata (make-hash-table :test 'equal) :type hash-table))

(defstruct aggregated-hook-result
  "Aggregated results from multiple hook executions."
  (results nil :type list))

(defun aggregated-hook-result-success-p (aggregated)
  "Check if all hooks succeeded."
  (every #'hook-result-success (aggregated-hook-result-results aggregated)))

(defun aggregated-hook-result-blocked-p (aggregated)
  "Check if any hook blocked the event."
  (some #'hook-result-blocked (aggregated-hook-result-results aggregated)))

;;; ============================================================================
;;; Hook Definition
;;; ============================================================================

(defclass hook-definition ()
  ((type
    :initarg :type
    :initform ""
    :reader hook-type
    :type string
    :documentation "Type of hook (PreToolUse, PostToolUse, Stop)")
   (matcher
    :initarg :matcher
    :initform nil
    :reader hook-matcher
    :type (or null string)
    :documentation "Glob pattern to match against tool name or event")
   (block-on-failure
    :initarg :block-on-failure
    :initform nil
    :reader hook-block-on-failure
    :type boolean
    :documentation "Whether to block the event on hook failure")
   (timeout-seconds
    :initarg :timeout-seconds
    :initform 30
    :reader hook-timeout-seconds
    :type number
    :documentation "Timeout for hook execution in seconds")))

(defclass command-hook-definition (hook-definition)
  ((command
    :initarg :command
    :initform ""
    :reader hook-command
    :type string
    :documentation "Shell command to execute")))

(defclass http-hook-definition (hook-definition)
  ((url
    :initarg :url
    :initform ""
    :reader hook-url
    :type string
    :documentation "URL to POST to")
   (headers
    :initarg :headers
    :initform nil
    :reader hook-headers
    :type list
    :documentation "HTTP headers to include")))

(defclass prompt-hook-definition (hook-definition)
  ((prompt
    :initarg :prompt
    :initform ""
    :reader hook-prompt
    :type string
    :documentation "Prompt template to send to LLM")
   (model
    :initarg :model
    :initform nil
    :reader hook-model
    :type (or null string)
    :documentation "Model to use for prompt evaluation")))

(defclass agent-hook-definition (hook-definition)
  ((prompt
    :initarg :prompt
    :initform ""
    :reader hook-prompt
    :type string
    :documentation "Prompt template to send to LLM")
   (model
    :initarg :model
    :initform nil
    :reader hook-model
    :type (or null string)
    :documentation "Model to use for prompt evaluation")))

(defun make-command-hook (type command &key (matcher nil) (block-on-failure nil) (timeout-seconds 30))
  "Create a command hook definition."
  (make-instance 'command-hook-definition
                 :type type
                 :command command
                 :matcher matcher
                 :block-on-failure block-on-failure
                 :timeout-seconds timeout-seconds))

(defun make-http-hook (type url &key (matcher nil) (block-on-failure nil) (timeout-seconds 30) (headers nil))
  "Create an HTTP hook definition."
  (make-instance 'http-hook-definition
                 :type type
                 :url url
                 :matcher matcher
                 :block-on-failure block-on-failure
                 :timeout-seconds timeout-seconds
                 :headers headers))

(defun make-prompt-hook (type prompt &key (matcher nil) (block-on-failure nil) (model nil))
  "Create a prompt hook definition."
  (make-instance 'prompt-hook-definition
                 :type type
                 :prompt prompt
                 :matcher matcher
                 :block-on-failure block-on-failure
                 :model model))

(defun make-agent-hook (type prompt &key (matcher nil) (block-on-failure nil) (model nil))
  "Create an agent hook definition."
  (make-instance 'agent-hook-definition
                 :type type
                 :prompt prompt
                 :matcher matcher
                 :block-on-failure block-on-failure
                 :model model))

;;; ============================================================================
;;; Hook Registry
;;; ============================================================================

(defstruct hook-registry
  "Registry of hooks by event type."
  (hooks (make-hash-table :test 'equal) :type hash-table))

(defun hook-registry-add (registry hook)
  "Add a hook to the registry."
  (let* ((type (hook-type hook))
         (existing (gethash type (hook-registry-hooks registry) nil)))
    (setf (gethash type (hook-registry-hooks registry))
          (if existing
              (cons hook existing)
              (list hook))))
  registry)

(defun hook-registry-get (registry event)
  "Get all hooks for an event."
  (gethash event (hook-registry-hooks registry) nil))

(defun hook-registry-remove (registry event hook)
  "Remove a hook from the registry."
  (let ((hooks (gethash event (hook-registry-hooks registry) nil)))
    (setf (gethash event (hook-registry-hooks registry))
          (remove hook hooks)))
  registry)

(defun hook-registry-clear (registry)
  "Clear all hooks from the registry."
  (clrhash (hook-registry-hooks registry))
  registry)

;;; ============================================================================
;;; Hook Execution Context
;;; ============================================================================

(defstruct hook-execution-context
  "Context passed into hook execution."
  (cwd nil :type (or null pathname string))
  (api-client nil :type (or null t))
  (default-model "" :type string))

;;; ============================================================================
;;; Hook Executor
;;; ============================================================================

(defstruct hook-executor
  "Execute hooks for lifecycle events."
  (registry nil :type (or null hook-registry))
  (context nil :type (or null hook-execution-context)))

(defun hook-executor-update-registry (executor registry)
  "Replace the active hook registry."
  (setf (hook-executor-registry executor) registry))

(defun hook-executor-update-context (executor &key (api-client nil) (default-model nil))
  "Update the active hook execution context."
  (when api-client
    (setf (hook-execution-context-api-client (hook-executor-context executor))
          api-client))
  (when default-model
    (setf (hook-execution-context-default-model (hook-executor-context executor))
          default-model)))

(defun hook-execute (executor event payload)
  "Execute all matching hooks for an event."
  (let ((results nil)
        (registry (hook-executor-registry executor))
        (context (hook-executor-context executor)))
    (dolist (hook (hook-registry-get registry event))
      (when (matches-hook-p hook payload)
        (let ((result (execute-hook hook event payload context)))
          (push result results))))
    (make-aggregated-hook-result :results (nreverse results))))

(defun matches-hook-p (hook payload)
  "Check if a hook matches the payload."
  (let ((matcher (hook-matcher hook)))
    (if matcher
        (let ((subject (or (gethash "tool_name" payload)
                          (gethash "prompt" payload)
                          (gethash "event" payload)
                          "")))
          (strings-match-p subject matcher))
        t)))

(defun execute-hook (hook event payload context)
  "Execute a single hook."
  (etypecase hook
    (command-hook-definition
     (execute-command-hook hook event payload context))
    (http-hook-definition
     (execute-http-hook hook event payload context))
    (prompt-hook-definition
     (execute-prompt-hook hook event payload context nil))
    (agent-hook-definition
     (execute-prompt-hook hook event payload context t))))

(defun execute-command-hook (hook event payload context)
  "Execute a command hook."
  (let* ((command (inject-arguments (hook-command hook) payload t))
         (success nil)
         (output "")
         (reason ""))
    (handler-case
        (let ((process (run-program "bash" (list "-c" command)
                                    :output :stream
                                    :error-output :stream
                                    :wait nil)))
          (sleep (hook-timeout-seconds hook))
          (let ((stdout (make-string-output-stream))
                (stderr (make-string-output-stream)))
            (copy-stream (process-output process) stdout)
            (copy-stream (process-error process) stderr)
            (setf output (format nil "~a~%~a"
                                (get-output-stream-string stdout)
                                (get-output-stream-string stderr)))
            (setf success (zerop (process-exit-code process)))))
      (error (e)
        (setf reason (format nil "Error: ~a" e))
        (setf success nil)))
    (make-hook-result
     (hook-type hook)
     success
     :output (if success output reason)
     :blocked (and (hook-block-on-failure hook) (not success))
     :reason (if success "" reason))))

(defun execute-http-hook (hook event payload context)
  "Execute an HTTP hook."
  (let ((success nil)
        (output "")
        (reason ""))
    (handler-case
        (let ((response (dexador:post
                         (hook-url hook)
                         :content (jonathan:to-json (list :event event :payload payload))
                         :headers (hook-headers hook)
                         :timeout (hook-timeout-seconds hook))))
          (setf output (if (stringp response) response (jonathan:to-json response)))
          (setf success t))
      (error (e)
        (setf reason (format nil "HTTP error: ~a" e))
        (setf success nil)))
    (make-hook-result
     (hook-type hook)
     success
     :output output
     :blocked (and (hook-block-on-failure hook) (not success))
     :reason reason)))

(defun execute-prompt-hook (hook event payload context agent-mode-p)
  "Execute a prompt or agent hook."
  (let* ((prompt (inject-arguments (hook-prompt hook) payload))
         (prefix (if agent-mode-p
                     "You are validating whether a hook condition passes. Be thorough. Return JSON: {\"ok\": true} or {\"ok\": false, \"reason\": \"...\"}."
                     "You are validating whether a hook condition passes. Return JSON: {\"ok\": true} or {\"ok\": false, \"reason\": \"...\"}."))
         (result nil)
         (success nil)
         (output "")
         (reason ""))
    ;; Note: This would need an actual API client to work
    ;; For now, we just return a placeholder
    (declare (ignore prefix))
    (setf output prompt)
    (setf success t)
    (make-hook-result
     (hook-type hook)
     success
     :output output
     :blocked (and (hook-block-on-failure hook) (not success))
     :reason reason)))

(defun inject-arguments (template payload &optional (shell-escape nil))
  "Inject payload arguments into a template string."
  (let ((serialized (jonathan:to-json payload)))
    (if shell-escape
        (format nil "'~a'" serialized)
        (substitute-all "$ARGUMENTS" serialized template))))

(defun substitute-all (old new string)
  "Replace all occurrences of OLD with NEW in STRING."
  (let ((old-len (length old))
        (result ""))
    (loop :for i :below (length string)
          :if (and (<= (+ i old-len) (length string))
                   (string= string old :start1 i :end1 (+ i old-len)))
            :do (progn
                  (setf result (concatenate 'string result new))
                  (incf i (1- old-len)))
          :else :do (setf result (concatenate 'string result (string (char string i)))))
    result))

;;; ============================================================================
;;; Hook Helpers
;;; ============================================================================

(defun parse-hook-json (text)
  "Parse hook JSON response."
  (handler-case
      (let ((parsed (jonathan:parse text)))
        (if (and (alist-p parsed)
                 (gethash "ok" parsed))
            parsed
            (list (cons "ok" nil)
                  (cons "reason" (or (gethash "reason" parsed) "hook returned invalid JSON")))))
    (error (e)
      (let ((lowered (string-trim '(#\Space #\Tab #\Newline) text)))
        (if (member lowered '("ok" "true" "yes") :test #'string-equal)
            (list (cons "ok" t))
            (list (cons "ok" nil)
                  (cons "reason" (or lowered "hook returned invalid JSON"))))))))
