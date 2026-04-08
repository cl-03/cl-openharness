;;; permissions.lisp - Permission checking for tool execution
;;;
;;; This file defines permission modes, settings, checker, and decisions
;;; for controlling tool execution safety.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Permission Modes
;;; ============================================================================

(defparameter +permission-mode-auto+ "auto"
  "Allow all tool executions without confirmation.")

(defparameter +permission-mode-default+ "default"
  "Require confirmation for mutating tools.")

(defparameter +permission-mode-plan+ "plan"
  "Block all mutating tools.")

;;; ============================================================================
;;; Sensitive Path Patterns
;;; ============================================================================

(defparameter *sensitive-path-patterns*
  '("*/.ssh/*"
    "*/.aws/credentials"
    "*/.aws/config"
    "*/.config/gcloud/*"
    "*/.azure/*"
    "*/.gnupg/*"
    "*/.docker/config.json"
    "*/.kube/config"
    "*/.openharness/credentials.json"
    "*/.openharness/copilot_auth.json"
    "*/.claude/.credentials.json"
    "*/.cl-openharness/credentials.json")
  "Paths that are always denied regardless of permission mode.
   These protect high-value credential and key material.")

;;; ============================================================================
;;; Permission Settings
;;; ============================================================================

(defstruct permission-settings
  "Configuration for permission checking."
  (mode +permission-mode-default+ :type string)
  (denied-tools nil :type list)
  (allowed-tools nil :type list)
  (denied-commands nil :type list)
  (path-rules nil :type list))

(defun create-permission-settings (&key (mode +permission-mode-default+)
                                      (denied-tools nil)
                                      (allowed-tools nil)
                                      (denied-commands nil)
                                      (path-rules nil))
  "Create permission settings."
  (make-permission-settings :mode mode
                           :denied-tools denied-tools
                           :allowed-tools allowed-tools
                           :denied-commands denied-commands
                           :path-rules path-rules))

;;; ============================================================================
;;; Path Rule
;;; ============================================================================

(defstruct path-rule
  "A glob-based path permission rule."
  (pattern "" :type string)
  (allow t :type boolean))

(defun create-path-rule (pattern &optional (allow t))
  "Create a path rule."
  (make-path-rule :pattern pattern :allow allow))

;;; ============================================================================
;;; Permission Decision
;;; ============================================================================

(defstruct permission-decision
  "Result of checking whether a tool invocation may run."
  (allowed nil :type boolean)
  (requires-confirmation nil :type boolean)
  (reason "" :type string))

(defun create-permission-decision (allowed &key (requires-confirmation nil) (reason ""))
  "Create a permission decision."
  (make-permission-decision :allowed allowed
                           :requires-confirmation requires-confirmation
                           :reason reason))

;;; ============================================================================
;;; Permission Checker
;;; ============================================================================

(defstruct permission-checker
  "Evaluate tool usage against configured permission mode and rules."
  (settings nil :type (or null permission-settings))
  (path-rules nil :type list))

(defun create-permission-checker (settings)
  "Create a permission checker from settings."
  (let ((checker (make-permission-checker :settings settings
                                          :path-rules nil)))
    ;; Parse path rules from settings
    (setf (permission-checker-path-rules checker)
          (mapcar (lambda (rule)
                    (if (path-rule-p rule)
                        rule
                        (make-path-rule :pattern (getf rule :pattern)
                                       :allow (getf rule :allow))))
                  (permission-settings-path-rules settings)))
    checker))

(defun match-path-pattern (path pattern)
  "Match a path against a glob pattern using fnmatch-style matching."
  (let ((path-parts (split-sequence:split-sequence #\/ path))
        (pattern-parts (split-sequence:split-sequence #\/ pattern)))
    (labels ((match-segment (path-seg pattern-seg)
               (cond
                 ((string= pattern-seg "**") t)
                 ((string= pattern-seg "*") t)
                 ((string-equal path-seg pattern-seg) t)
                 ((and (string= pattern-seg "*.*")
                       (find #\. path-seg)) t)
                 (t (string-equal path-seg pattern-seg)))))
      (if (= (length path-parts) (length pattern-parts))
          (every #'match-segment path-parts pattern-parts)
          ;; Handle ** patterns
          (match-pattern-with-wildcards path-parts pattern-parts)))))

(defun match-pattern-with-wildcards (path-parts pattern-parts)
  "Match path parts against pattern parts with ** support."
  (cond
    ((and (null path-parts) (null pattern-parts)) t)
    ((null path-parts) nil)
    ((null pattern-parts) nil)
    ((string= (car pattern-parts) "**")
     (or (match-pattern-with-wildcards (cdr path-parts) pattern-parts)
         (match-pattern-with-wildcards (cdr path-parts) (cdr pattern-parts))))
    ((string= (car pattern-parts) "*")
     (match-pattern-with-wildcards (cdr path-parts) (cdr pattern-parts)))
    ((string-equal (car path-parts) (car pattern-parts))
     (match-pattern-with-wildcards (cdr path-parts) (cdr pattern-parts)))
    (t nil)))

(defun match-sensitive-path-p (path)
  "Check if path matches any sensitive pattern."
  (dolist (pattern *sensitive-path-patterns* nil)
    (when (match-path-pattern path pattern)
      (return-from match-sensitive-path-p t))))

(defun permission-evaluate (checker tool-name &key is-read-only file-path command)
  "Return whether the tool may run immediately."
  (let ((result nil))
    (cond
      ;; Built-in sensitive path protection
      ((and file-path (match-sensitive-path-p file-path))
       (setf result (create-permission-decision
                     nil
                     :reason (format nil "Access denied: ~a is a sensitive credential path"
                                     file-path))))
      ;; Explicit tool deny list
      ((member tool-name (permission-settings-denied-tools
                          (permission-checker-settings checker))
               :test #'string=)
       (setf result (create-permission-decision
                     nil
                     :reason (format nil "~a is explicitly denied" tool-name))))
      ;; Explicit tool allow list
      ((member tool-name (permission-settings-allowed-tools
                          (permission-checker-settings checker))
               :test #'string=)
       (setf result (create-permission-decision
                     t
                     :reason (format nil "~a is explicitly allowed" tool-name))))
      ;; Read-only tools always allowed
      (is-read-only
       (setf result (create-permission-decision
                     t
                     :reason "read-only tools are allowed")))
      ;; Full auto: allow everything
      ((string= (permission-settings-mode
                 (permission-checker-settings checker))
                +permission-mode-auto+)
       (setf result (create-permission-decision
                     t
                     :reason "Auto mode allows all tools")))
      ;; Plan mode: block mutating tools
      ((string= (permission-settings-mode
                 (permission-checker-settings checker))
                +permission-mode-plan+)
       (setf result (create-permission-decision
                     nil
                     :reason "Plan mode blocks mutating tools until the user exits plan mode")))
      (t
       ;; Default mode: require confirmation for mutating tools
       (setf result (create-permission-decision
                     nil
                     :requires-confirmation t
                     :reason "Mutating tools require user confirmation in default mode"))))
    result))

;;; ============================================================================
;;; Permission Helpers
;;; ============================================================================

(defun permission-require-confirmation-p (decision)
  "Check if permission decision requires user confirmation."
  (permission-decision-requires-confirmation decision))

(defun permission-allowed-p (decision)
  "Check if permission decision allows the tool."
  (permission-decision-allowed decision))

(defun permission-denied-p (decision)
  "Check if permission decision denies the tool."
  (not (or (permission-decision-allowed decision)
           (permission-decision-requires-confirmation decision))))
