;;; runtime.lisp - Runtime bundle and harness
;;;
;;; This file defines the main harness interface that ties together
;;; all subsystems into a coherent agent runtime.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Runtime Bundle
;;; ============================================================================

(defstruct runtime-bundle
  "Complete runtime configuration for the harness."
  (api-key nil :type (or null string))
  (auth-token nil :type (or null string))
  (base-url nil :type (or null string))
  (model "" :type string)
  (system-prompt "" :type string)
  (cwd nil :type (or null pathname string))
  (max-tokens 4096 :type integer)
  (max-turns nil :type (or null integer))
  (permission-mode "default" :type string)
  (denied-tools nil :type list)
  (allowed-tools nil :type list)
  (denied-commands nil :type list)
  (path-rules nil :type list)
  (hooks nil :type list)
  (tools nil :type list))

(defun make-runtime-bundle (&key api-key auth-token base-url
                                 (model "claude-sonnet-4-6")
                                 (system-prompt "You are a helpful AI assistant.")
                                 (cwd (uiop:getcwd))
                                 (max-tokens 4096)
                                 (max-turns nil)
                                 (permission-mode "default")
                                 (denied-tools nil)
                                 (allowed-tools nil)
                                 (denied-commands nil)
                                 (path-rules nil)
                                 (hooks nil)
                                 (tools nil))
  "Create a runtime bundle."
  (make-runtime-bundle :api-key api-key
                       :auth-token auth-token
                       :base-url base-url
                       :model model
                       :system-prompt system-prompt
                       :cwd cwd
                       :max-tokens max-tokens
                       :max-turns max-turns
                       :permission-mode permission-mode
                       :denied-tools denied-tools
                       :allowed-tools allowed-tools
                       :denied-commands denied-commands
                       :path-rules path-rules
                       :hooks hooks
                       :tools tools))

;;; ============================================================================
;;; Harness
;;; ============================================================================

(defstruct harness
  "Main harness interface that ties together all subsystems."
  (query-engine nil :type (or null query-engine))
  (runtime-bundle nil :type (or null runtime-bundle))
  (running nil :type boolean)
  (session-id "" :type string))

(defun make-harness (&key (bundle nil))
  "Create a new harness instance."
  (let ((harness (make-harness :runtime-bundle bundle
                               :running nil
                               :session-id (generate-uuid))))
    (when bundle
      (setf (harness-query-engine harness)
            (initialize-harness-engine bundle harness)))
    harness))

(defun initialize-harness-engine (bundle harness)
  "Initialize the query engine from a runtime bundle."
  ;; Create API client
  (let ((api-client (make-api-client
                     :api-key (runtime-bundle-api-key bundle)
                     :auth-token (runtime-bundle-auth-token bundle)
                     :base-url (runtime-bundle-base-url bundle))))

    ;; Create tool registry
    (let ((tool-registry (make-tool-registry)))
      (register-builtin-tools tool-registry)
      ;; Register custom tools
      (dolist (tool (runtime-bundle-tools bundle))
        (tool-registry-register tool-registry tool)))

    ;; Create permission settings and checker
    (let ((permission-settings
           (make-permission-settings
            :mode (runtime-bundle-permission-mode bundle)
            :denied-tools (runtime-bundle-denied-tools bundle)
            :allowed-tools (runtime-bundle-allowed-tools bundle)
            :denied-commands (runtime-bundle-denied-commands bundle)
            :path-rules (runtime-bundle-path-rules bundle)))
          (permission-checker (make-permission-checker permission-settings)))

      ;; Create hook registry and executor
      (let ((hook-registry (make-hook-registry))
            (hook-executor nil))
        ;; Register hooks
        (dolist (hook (runtime-bundle-hooks bundle))
          (hook-registry-add hook-registry hook))

        (setf hook-executor (make-hook-executor
                             hook-registry
                             (make-hook-execution-context
                              (runtime-bundle-cwd bundle)
                              api-client
                              (runtime-bundle-model bundle))))

        ;; Create query engine
        (make-query-engine
         :api-client api-client
         :tool-registry tool-registry
         :permission-checker permission-checker
         :cwd (runtime-bundle-cwd bundle)
         :model (runtime-bundle-model bundle)
         :system-prompt (runtime-bundle-system-prompt bundle)
         :max-tokens (runtime-bundle-max-tokens bundle)
         :max-turns (runtime-bundle-max-turns bundle)
         :hook-executor hook-executor)))))

(defun harness-run (harness prompt)
  "Run the harness with a user prompt."
  (setf (harness-running harness) t)
  (let ((query-engine (harness-query-engine harness)))
    (when query-engine
      (multiple-value-bind (events usage)
          (query-engine-submit query-engine prompt)
        (values events usage)))))

(defun harness-continue (harness &optional (max-turns nil))
  "Continue the harness after a pending tool execution."
  (let ((query-engine (harness-query-engine harness)))
    (when query-engine
      (multiple-value-bind (events usage)
          (query-engine-continue query-engine max-turns)
        (values events usage)))))

(defun harness-stop (harness)
  "Stop the harness."
  (setf (harness-running harness) nil)
  ;; Run Stop hooks
  (let ((hook-executor (query-engine-hook-executor (harness-query-engine harness))))
    (when hook-executor
      (hook-execute hook-executor +hook-event-stop+
                    (list (cons "session_id" (harness-session-id harness))))))
  harness)

(defun harness-clear (harness)
  "Clear the harness conversation history."
  (let ((query-engine (harness-query-engine harness)))
    (when query-engine
      (query-engine-clear query-engine)))
  harness)

(defun harness-messages (harness)
  "Get the harness conversation history."
  (query-engine-messages (harness-query-engine harness)))

(defun harness-usage (harness)
  "Get the harness token usage."
  (query-engine-total-usage (harness-query-engine harness)))

;;; ============================================================================
;;; With Harness Macro
;;; ============================================================================

(defmacro with-harness ((harness-var &key api-key auth-token base-url
                                        (model "claude-sonnet-4-6")
                                        (system-prompt "You are a helpful AI assistant.")
                                        (cwd (uiop:getcwd))
                                        (max-tokens 4096)
                                        (max-turns nil)
                                        (permission-mode "default")
                                        (denied-tools nil)
                                        (allowed-tools nil)
                                        (denied-commands nil)
                                        (path-rules nil)
                                        (hooks nil)
                                        (tools nil))
                        &body body)
  "Create and run a harness within a dynamic extent."
  `(let ((,harness-var (make-harness
                        :bundle (make-runtime-bundle
                                 :api-key ,api-key
                                 :auth-token ,auth-token
                                 :base-url ,base-url
                                 :model ,model
                                 :system-prompt ,system-prompt
                                 :cwd ,cwd
                                 :max-tokens ,max-tokens
                                 :max-turns ,max-turns
                                 :permission-mode ,permission-mode
                                 :denied-tools ,denied-tools
                                 :allowed-tools ,allowed-tools
                                 :denied-commands ,denied-commands
                                 :path-rules ,path-rules
                                 :hooks ,hooks
                                 :tools ,tools))))
     (unwind-protect
          (progn ,@body)
       (harness-stop ,harness-var))))

;;; ============================================================================
;;; Simple Run Function
;;; ============================================================================

(defun harness-run-single (prompt &key api-key auth-token base-url
                                  (model "claude-sonnet-4-6")
                                  (system-prompt "You are a helpful AI assistant.")
                                  (cwd (uiop:getcwd))
                                  (max-tokens 4096)
                                  (max-turns 10)
                                  (permission-mode "default"))
  "Run a single prompt through the harness and return the response."
  (with-harness (h :api-key api-key
                   :auth-token auth-token
                   :base-url base-url
                   :model model
                   :system-prompt system-prompt
                   :cwd cwd
                   :max-tokens max-tokens
                   :max-turns max-turns
                   :permission-mode permission-mode)
    (multiple-value-bind (events usage)
        (harness-run h prompt)
      (declare (ignore usage))
      ;; Extract final text response
      (let ((text nil))
        (dolist (event events)
          (when (api-text-delta-event-p event)
            (push (api-text-delta-event-text event) text)))
        (apply #'concatenate 'string (nreverse text))))))

;;; ============================================================================
;;; Harness State
;;; ============================================================================

(defparameter *default-harness* nil
  "Default harness instance for simple usage.")

(defun with-default-harness (fn &key api-key auth-token base-url
                                    (model "claude-sonnet-4-6")
                                    (system-prompt "You are a helpful AI assistant.")
                                    (cwd (uiop:getcwd)))
  "Set the default harness and call fn with it."
  (let ((harness (make-harness
                  :bundle (make-runtime-bundle
                           :api-key api-key
                           :auth-token auth-token
                           :base-url base-url
                           :model model
                           :system-prompt system-prompt
                           :cwd cwd))))
    (setf *default-harness* harness)
    (unwind-protect
         (funcall fn harness)
      (setf *default-harness* nil))))

(defun get-default-harness ()
  "Get the default harness instance."
  (or *default-harness*
      (error "No default harness set. Call with-default-harness first.")))

;;; ============================================================================
;;; Session Management
;;; ============================================================================

(defstruct session-info
  "Information about a harness session."
  (id "" :type string)
  (created-at 0 :type integer)
  (last-activity 0 :type integer)
  (message-count 0 :type integer)
  (total-input-tokens 0 :type integer)
  (total-output-tokens 0 :type integer))

(defun make-session-info (&key (id (generate-uuid)))
  "Create a session info."
  (make-session-info :id id
                     :created-at (get-universal-time)
                     :last-activity (get-universal-time)))

(defun session-info-record-activity (info)
  "Record activity in a session."
  (incf (session-info-message-count info))
  (setf (session-info-last-activity info) (get-universal-time))
  info)

(defun session-info-record-usage (info input-tokens output-tokens)
  "Record token usage in a session."
  (incf (session-info-total-input-tokens info) input-tokens)
  (incf (session-info-total-output-tokens info) output-tokens)
  (session-info-record-activity info))
