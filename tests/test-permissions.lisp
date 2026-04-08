;;; test-permissions.lisp - Tests for permissions.lisp
;;;
;;; This file contains tests for the permission checker.

(in-package #:cl-openharness/test)

(defun test-permission-settings-create ()
  "Test permission settings creation."
  (let ((settings (make-permission-settings)))
    (assert (permission-settings-p settings))
    (assert (string= (permission-settings-mode settings) "default"))
    :pass))

(defun test-permission-checker-create ()
  "Test permission checker creation."
  (let ((settings (make-permission-settings))
        (checker (make-permission-checker settings)))
    (assert (permission-checker-p checker))
    :pass))

(defun test-permission-auto-mode ()
  "Test auto mode allows everything."
  (let ((settings (make-permission-settings :mode "auto"))
        (checker (make-permission-checker (make-permission-settings :mode "auto"))))
    (let ((decision (permission-evaluate checker "Write" :is-read-only nil)))
      (assert (permission-allowed-p decision))
      :pass)))

(defun test-permission-plan-mode ()
  "Test plan mode blocks mutating tools."
  (let ((checker (make-permission-checker (make-permission-settings :mode "plan"))))
    (let ((decision (permission-evaluate checker "Write" :is-read-only nil)))
      (assert (not (permission-allowed-p decision)))
      (assert (not (permission-decision-requires-confirmation decision)))
      :pass)))

(defun test-permission-read-only ()
  "Test read-only tools are allowed in default mode."
  (let ((checker (make-permission-checker (make-permission-settings :mode "default"))))
    (let ((decision (permission-evaluate checker "Read" :is-read-only t)))
      (assert (permission-allowed-p decision))
      :pass)))

(defun test-permission-default-requires-confirmation ()
  "Test default mode requires confirmation for mutating tools."
  (let ((checker (make-permission-checker (make-permission-settings :mode "default"))))
    (let ((decision (permission-evaluate checker "Write" :is-read-only nil)))
      (assert (not (permission-allowed-p decision)))
      (assert (permission-decision-requires-confirmation decision))
      :pass)))

(defun test-permission-explicit-deny ()
  "Test explicit tool deny list."
  (let ((checker (make-permission-checker
                  (make-permission-settings :denied-tools '("DangerousTool")))))
    (let ((decision (permission-evaluate checker "DangerousTool" :is-read-only t)))
      (assert (not (permission-allowed-p decision)))
      :pass)))

(defun test-permission-explicit-allow ()
  "Test explicit tool allow list."
  (let ((checker (make-permission-checker
                  (make-permission-settings :allowed-tools '("SpecialTool")))))
    (let ((decision (permission-evaluate checker "SpecialTool" :is-read-only nil)))
      (assert (permission-allowed-p decision))
      :pass)))

(defun test-permission-sensitive-path ()
  "Test sensitive path patterns are always denied."
  (let ((checker (make-permission-checker (make-permission-settings))))
    ;; Test SSH credentials path
    (let ((decision (permission-evaluate checker "Read" :is-read-only t
                                         :file-path "/home/user/.ssh/id_rsa")))
      (assert (not (permission-allowed-p decision)))
      (assert (cl-mismatch (permission-decision-reason decision) "sensitive credential path")))
    ;; Test AWS credentials
    (let ((decision (permission-evaluate checker "Read" :is-read-only t
                                         :file-path "/home/user/.aws/credentials")))
      (assert (not (permission-allowed-p decision))))
    :pass))

(defun test-permission-path-rules ()
  "Test path-level rules."
  (let ((checker (make-permission-checker
                  (make-permission-settings
                   :path-rules (list (make-path-rule "/allowed/*" t)
                                    (make-path-rule "/denied/*" nil))))))
    ;; Allowed path
    (let ((decision (permission-evaluate checker "Write" :is-read-only nil
                                         :file-path "/allowed/file.txt")))
      (assert (permission-allowed-p decision)))
    ;; Denied path
    (let ((decision (permission-evaluate checker "Write" :is-read-only nil
                                         :file-path "/denied/file.txt")))
      (assert (not (permission-allowed-p decision))))
    :pass))

(defun test-match-path-pattern ()
  "Test path pattern matching."
  ;; Exact match
  (assert (match-path-pattern "/foo/bar" "/foo/bar"))
  ;; Single wildcard
  (assert (match-path-pattern "/foo/bar" "/foo/*"))
  ;; Double wildcard
  (assert (match-path-pattern "/foo/bar/baz" "/foo/**"))
  ;; No match
  (assert (not (match-path-pattern "/foo/bar" "/baz/bar")))
  :pass))

(defun run-permissions-tests ()
  "Run all permissions tests."
  (let ((passed 0)
        (failed 0))
    (dolist (test '(test-permission-settings-create
                    test-permission-checker-create
                    test-permission-auto-mode
                    test-permission-plan-mode
                    test-permission-read-only
                    test-permission-default-requires-confirmation
                    test-permission-explicit-deny
                    test-permission-explicit-allow
                    test-permission-sensitive-path
                    test-permission-path-rules
                    test-match-path-pattern))
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
