;;; api-client.lisp - Anthropic-compatible API client
;;;
;;; This file defines the API client wrapper with retry logic for
;;; Anthropic-compatible endpoints.

(in-package #:cl-openharness)

;;; ============================================================================
;;; API Events
;;; ============================================================================

(defstruct api-text-delta-event
  "Incremental text produced by the model."
  (text "" :type string))

(defstruct api-message-complete-event
  "Terminal event containing the full assistant message."
  (message nil :type (or null conversation-message))
  (usage nil :type (or null alist))
  (stop-reason nil :type (or null string)))

(defstruct api-retry-event
  "A recoverable upstream failure that will be retried automatically."
  (message "" :type string)
  (attempt 0 :type integer)
  (max-attempts 0 :type integer)
  (delay-seconds 0 :type number))

;;; ============================================================================
;;; API Request
;;; ============================================================================

(defstruct api-request
  "Input parameters for a model invocation."
  (model "" :type string)
  (messages nil :type list)
  (system-prompt nil :type (or null string))
  (max-tokens 4096 :type integer)
  (tools nil :type list))

(defun make-api-request (model messages &key system-prompt (max-tokens 4096) (tools nil))
  "Create an API request."
  (make-api-request :model model
                    :messages messages
                    :system-prompt system-prompt
                    :max-tokens max-tokens
                    :tools tools))

;;; ============================================================================
;;; API Client
;;; ============================================================================

(defstruct api-client
  "Anthropic API client wrapper with retry logic."
  (api-key nil :type (or null string))
  (auth-token nil :type (or null string))
  (base-url nil :type (or null string))
  (claude-oauth nil :type boolean)
  (session-id "" :type string))

(defun make-api-client (&key (api-key nil) (auth-token nil) (base-url nil) (claude-oauth nil))
  "Create an API client."
  (make-api-client :api-key api-key
                   :auth-token auth-token
                   :base-url base-url
                   :claude-oauth claude-oauth
                   :session-id (if claude-oauth (generate-uuid) "")))

(defparameter *api-max-retries* 3
  "Maximum number of retry attempts for API calls.")

(defparameter *api-base-delay* 1.0
  "Base delay in seconds for exponential backoff.")

(defparameter *api-max-delay* 30.0
  "Maximum delay in seconds for retries.")

(defparameter *api-retryable-status-codes*
  '(429 500 502 503 529)
  "HTTP status codes that should trigger a retry.")

(defun api-client-stream-message (client request)
  "Yield streamed events for the request.
   Returns multiple values: (event usage)"
  (let ((attempt 0)
        (last-error nil))
    (loop
      (handler-case
          (return-from api-client-stream-message
            (api-client-stream-once client request))
        (error (exc)
          (setf last-error exc)
          (if (>= attempt *api-max-retries*)
              (error "API request failed after ~a attempts: ~a" (1+ *api-max-retries*) exc)
              (when (retryable-error-p exc)
                (let ((delay (calculate-api-retry-delay attempt exc)))
                  (log :warning "API request failed (attempt ~a/~a), retrying in ~as: ~a"
                       (1+ attempt) (1+ *api-max-retries*) delay exc)
                  (values (make-api-retry-event
                           :message (format nil "~a" exc)
                           :attempt (1+ attempt)
                           :max-attempts (1+ *api-max-retries*)
                           :delay-seconds delay)
                          nil)
                  (sleep delay)
                  (incf attempt)))))))))

(defun retryable-error-p (condition)
  "Check if an error is retryable."
  (let ((status-code (if (and (listp condition)
                              (getf condition :status-code))
                         (getf condition :status-code)
                         nil)))
    (if status-code
        (member status-code *api-retryable-status-codes*)
        t)))

(defun calculate-api-retry-delay (attempt &optional (exc nil))
  "Calculate delay with exponential backoff and jitter."
  (let* ((base *api-base-delay*)
         (max *api-max-delay*)
         (exponential-delay (* base (expt 2 attempt)))
         (capped-delay (min exponential-delay max))
         (jitter (* capped-delay 0.25 (random 1.0))))
    (+ capped-delay jitter)))

(defun api-client-stream-once (client request)
  "Single attempt at streaming a message."
  (let* ((url (or (api-client-base-url client)
                  "https://api.anthropic.com"))
         (endpoint (if (api-client-claude-oauth client)
                       "/v1/messages"
                       "/v1/messages"))
         (full-url (concatenate 'string url endpoint))
         (headers (build-api-headers client))
         (body (build-api-body client request)))
    (handler-case
        (let ((response (dexador:post
                         full-url
                         :headers headers
                         :content (jonathan:json body)
                         :content-type "application/json"
                         :return :content)))
          (parse-api-response response))
      (dexador.error:http-status-failed (e)
        (let ((status (dexador.response:status e.response)))
          (if (member status *api-retryable-status-codes*)
              (error e)
              (error "API error: ~a (~a)" (dexador.response:body e.response) status))))
      (error (e)
        (error "API request failed: ~a" e)))))

(defun build-api-headers (client)
  "Build API request headers."
  (let ((headers '(("Content-Type" . "application/json"))))
    (if (api-client-api-key client)
        (push (cons "x-api-key" (api-client-api-key client)) headers)
        (when (api-client-auth-token client)
          (push (cons "Authorization" (format nil "Bearer ~a" (api-client-auth-token client))) headers)))
    (push (cons "anthropic-version" "2023-06-01") headers)
    (when (api-client-claude-oauth client)
      (push (cons "anthropic-beta" "oauth-2025-04-20") headers))
    headers))

(defun build-api-body (client request)
  "Build API request body."
  (let ((body `(("model" . ,(api-request-model request))
                ("max_tokens" . ,(api-request-max-tokens request))
                ("messages" . ,(messages-to-api-params (api-request-messages request))))))
    (when (api-request-system-prompt request)
      (push (cons "system" (api-request-system-prompt request)) body))
    (when (api-request-tools request)
      (push (cons "tools" (api-request-tools request)) body))
    (when (api-client-claude-oauth client)
      (push (cons "betas" '(("oauth-2025-04-20"))) body))
    body))

(defun parse-api-response (response)
  "Parse API response and return events."
  ;; This is a simplified parser - in practice you'd parse the full response
  (let ((message (assistant-message-from-api response)))
    (values (make-api-message-complete-event
             :message message
             :usage (getf response :usage)
             :stop-reason (getf response :stop_reason))
            (getf response :usage))))

;;; ============================================================================
;;; Usage Tracking
;;; ============================================================================

(defstruct usage-snapshot
  "Token usage snapshot."
  (input-tokens 0 :type integer)
  (output-tokens 0 :type integer))

(defun make-usage-snapshot (&key (input-tokens 0) (output-tokens 0))
  "Create a usage snapshot."
  (make-usage-snapshot :input-tokens input-tokens :output-tokens output-tokens))

(defstruct cost-tracker
  "Track token costs across conversation turns."
  (total (make-usage-snapshot) :type usage-snapshot))

(defun make-cost-tracker ()
  "Create a cost tracker."
  (make-cost-tracker))

(defun cost-tracker-add (tracker usage)
  "Add usage to the tracker."
  (incf (usage-snapshot-input-tokens (cost-tracker-total tracker))
        (usage-snapshot-input-tokens usage))
  (incf (usage-snapshot-output-tokens (cost-tracker-total tracker))
        (usage-snapshot-output-tokens usage))
  tracker)

(defun cost-tracker-total (tracker)
  "Return total usage."
  (cost-tracker-total tracker))

(defun cost-tracker-clear (tracker)
  "Clear the cost tracker."
  (setf (cost-tracker-total tracker) (make-usage-snapshot))
  tracker)
