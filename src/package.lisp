;;; package.lisp - Package definition for cl-openharness
;;;
;;; This file defines the main package and exports for the OpenHarness
;;; Common Lisp implementation.

(defpackage #:cl-openharness
  (:nicknames #:oh)
  (:use #:cl)
  (:export
   ;; Messages
   #:make-text-block
   #:text-block-p
   #:text-block-text
   #:make-tool-use-block
   #:tool-use-block-p
   #:tool-use-block-id
   #:tool-use-block-name
   #:tool-use-block-input
   #:make-tool-result-block
   #:tool-result-block-p
   #:tool-result-block-tool-use-id
   #:tool-result-block-content
   #:tool-result-block-is-error
   #:make-conversation-message
   #:conversation-message-p
   #:conversation-message-role
   #:conversation-message-content
   #:conversation-message-text
   #:conversation-message-tool-uses
   #:conversation-message-from-user-text
   #:message-to-api-param
   #:serialize-content-block

   ;; Tools
   #:base-tool
   #:tool-registry
   #:tool-execution-context
   #:tool-result
   #:make-tool-execution-context
   #:make-tool-result
   #:tool-result-output
   #:tool-result-is-error
   #:tool-result-metadata
   #:tool-name
   #:tool-description
   #:tool-input-model
   #:tool-execute
   #:tool-is-read-only
   #:tool-to-api-schema
   #:make-tool-registry
   #:tool-registry-register
   #:tool-registry-get
   #:tool-registry-list-tools
   #:tool-registry-to-api-schema

   ;; Permissions
   #:permission-mode
   #:permission-settings
   #:permission-decision
   #:permission-checker
   #:make-permission-settings
   #:make-permission-checker
   #:permission-evaluate
   #:permission-decision-allowed
   #:permission-decision-requires-confirmation
   #:permission-decision-reason
   #:*sensitive-path-patterns*

   ;; Hooks
   #:hook-event
   #:hook-definition
   #:hook-result
   #:hook-registry
   #:hook-executor
   #:make-hook-result
   #:make-hook-registry
   #:make-hook-executor
   #:hook-registry-add
   #:hook-registry-get
   #:hook-execute
   #:hook-result-success
   #:hook-result-output
   #:hook-result-blocked
   #:hook-result-reason

   ;; Query Engine
   #:query-context
   #:query-engine
   #:make-query-context
   #:make-query-engine
   #:query-engine-submit
   #:query-engine-continue
   #:query-engine-messages
   #:query-engine-clear
   #:query-engine-set-system-prompt
   #:query-engine-set-model

   ;; API Client
   #:api-client
   #:make-api-client
   #:api-client-stream-message
   #:api-request
   #:api-text-delta-event
   #:api-message-complete-event
   #:api-retry-event

   ;; Runtime
   #:harness
   #:make-harness
   #:harness-run
   #:harness-stop
   #:with-harness))

(in-package #:cl-openharness)
