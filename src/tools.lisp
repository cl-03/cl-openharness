;;; tools.lisp - Tool abstractions and registry
;;;
;;; This file defines the base tool class, tool execution context,
;;; tool result, and the tool registry.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Tool Execution Context
;;; ============================================================================

(defstruct tool-execution-context
  "Shared execution context for tool invocations."
  (cwd nil :type (or null pathname string))
  (metadata (make-hash-table :test 'equal) :type hash-table))

;;; ============================================================================
;;; Tool Result
;;; ============================================================================

(defstruct tool-result
  "Normalized tool execution result."
  (output "" :type string)
  (is-error nil :type boolean)
  (metadata (make-hash-table :test 'equal) :type hash-table))

;;; ============================================================================
;;; Base Tool Class
;;; ============================================================================

(defclass base-tool ()
  ((name
    :initarg :name
    :initform nil
    :reader tool-name
    :type (or null string)
    :documentation "Name of the tool")
   (description
    :initarg :description
    :initform ""
    :reader tool-description
    :type string
    :documentation "Description of what the tool does")
   (input-schema
    :initarg :input-schema
    :initform nil
    :reader tool-input-schema
    :type (or null list)
    :documentation "JSON schema for tool input")
   (read-only-p
    :initarg :read-only-p
    :initform nil
    :reader tool-read-only-p
    :type boolean
    :documentation "Whether the tool is read-only")))

(defgeneric tool-execute (tool arguments context)
  (:documentation "Execute the tool with the given arguments and context. Returns a tool-result structure.")
  (:method (tool arguments context)
    "Default method returns nil."
    (declare (ignore tool arguments context))
    nil))

(defgeneric tool-is-read-only (tool arguments)
  (:documentation "Return whether the tool invocation is read-only.")
  (:method (tool arguments)
    nil)
  (:method ((tool base-tool) arguments)
    (declare (ignore arguments))
    (tool-read-only-p tool)))

(defgeneric tool-to-api-schema (tool)
  (:documentation "Return the tool schema expected by the API.")
  (:method (tool)
    nil))

(defmethod tool-to-api-schema ((tool base-tool))
  `(("name" . ,(tool-name tool))
    ("description" . ,(tool-description tool))
    ("input_schema" . ,(or (tool-input-schema tool)
                           '(("type" . "object")
                             ("properties" . ()))))))

;;; ============================================================================
;;; Function Tool - Create tools from functions
;;; ============================================================================

(defclass function-tool (base-tool)
  ((handler
    :initarg :handler
    :initform nil
    :reader tool-handler
    :type (or null function)
    :documentation "Function that executes the tool")
   (read-only-handler
    :initarg :read-only-handler
    :initform nil
    :reader tool-read-only-handler
    :type (or null function)
    :documentation "Optional function to determine if invocation is read-only")))

(defun make-function-tool (name description handler &key input-schema (read-only-p nil))
  "Create a function-backed tool."
  (make-instance 'function-tool
                 :name name
                 :description description
                 :input-schema input-schema
                 :handler handler
                 :read-only-p read-only-p))

(defmethod tool-execute ((tool function-tool) arguments context)
  "Execute the function tool."
  (let ((handler (tool-handler tool)))
    (when handler
      (let ((result (funcall handler arguments context)))
        (etypecase result
          (tool-result result)
          (string (create-tool-result result))
          (list (create-tool-result (format nil "~a" result)))
          (t (create-tool-result (format nil "~a" result))))))))

(defmethod tool-is-read-only ((tool function-tool) arguments)
  "Check if function tool invocation is read-only."
  (let ((handler (tool-read-only-handler tool)))
    (if handler
        (funcall handler arguments)
        (tool-read-only-p tool))))

;;; ============================================================================
;;; Tool Registry
;;; ============================================================================

(defstruct tool-registry
  "Map tool names to implementations."
  (tools (make-hash-table :test 'equal) :type hash-table))

(defun create-tool-registry ()
  "Create a new tool registry."
  (make-tool-registry))

(defun tool-registry-register (registry tool)
  "Register a tool instance."
  (setf (gethash (tool-name tool) (tool-registry-tools registry))
        tool)
  registry)

(defun tool-registry-get (registry name)
  "Return a registered tool by name."
  (gethash name (tool-registry-tools registry)))

(defun tool-registry-list-tools (registry)
  "Return all registered tools as a list."
  (let (tools)
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v tools))
             (tool-registry-tools registry))
    tools))

(defun tool-registry-to-api-schema (registry)
  "Return all tool schemas in API format."
  (mapcar #'tool-to-api-schema (tool-registry-list-tools registry)))

(defun tool-registry-remove (registry name)
  "Remove a tool from the registry."
  (remhash name (tool-registry-tools registry)))

(defun tool-registry-has-p (registry name)
  "Check if a tool is registered."
  (nth-value 0 (gethash name (tool-registry-tools registry))))

;;; ============================================================================
;;; Built-in Tools
;;; ============================================================================

(defun register-builtin-tools (registry)
  "Register built-in tools to the registry."
  ;; Bash tool
  (tool-registry-register registry
    (make-function-tool
     "Bash"
     "Execute a shell command"
     (lambda (arguments context)
       (let ((command (gethash "command" arguments "")))
         (handler-case
             (let ((output (with-output-to-string (s)
                             (run-program "bash" (list "-c" command)
                                          :output s
                                          :error-output s))))
               (make-tool-result :output output))
           (error (e)
             (create-tool-result (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("command" . (("type" . "string")
                                                    ("description" . "The shell command to execute")))))
                     ("required" . ("command")))))

  ;; Read tool
  (tool-registry-register registry
    (make-function-tool
     "Read"
     "Read a file from the filesystem"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments "")))
         (handler-case
             (with-open-file (stream file-path :direction :input)
               (make-tool-result :output (let ((content (make-string (file-length stream))))
                                   (read-sequence content stream)
                                   content)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to read")))))
                     ("required" . ("file_path")))
     :read-only-p t))

  ;; Write tool
  (tool-registry-register registry
    (make-function-tool
     "Write"
     "Write a file to the filesystem"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments ""))
             (content (gethash "content" arguments "")))
         (handler-case
             (with-open-file (stream file-path :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
               (write-string content stream)
               (make-tool-result :output (format nil "Successfully wrote ~a" file-path)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to write")))
                                      ("content" . (("type" . "string")
                                                    ("description" . "The content to write to the file")))))
                     ("required" . ("file_path" "content")))))

  ;; Edit tool
  (tool-registry-register registry
    (make-function-tool
     "Edit"
     "Edit a file by replacing text"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments ""))
             (old-string (gethash "old_string" arguments ""))
             (new-string (gethash "new_string" arguments "")))
         (handler-case
             (with-open-file (stream file-path :direction :io)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 (if (search old-string content)
                     (progn
                       (file-position stream 0)
                       (write-string (cl-substitute new-string old-string content) stream)
                       (make-tool-result :output "Successfully edited file"))
                     (make-tool-result :output "Error: old_string not found in file" :is-error t))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to edit")))
                                      ("old-string" . (("type" . "string")
                                                       ("description" . "The text to replace")))
                                      ("new-string" . (("type" . "string")
                                                       ("description" . "The text to replace it with")))))
                     ("required" . ("file_path" "old_string" "new_string")))))

  ;; Glob tool
  (tool-registry-register registry
    (make-function-tool
     "Glob"
     "Find files matching a glob pattern"
     (lambda (arguments context)
       (let ((pattern (gethash "pattern" arguments "")))
         (handler-case
             (let ((files (directory pattern)))
               (make-tool-result :output (format nil "~{~a~%~}" files)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("pattern" . (("type" . "string")
                                                    ("description" . "The glob pattern to match files against")))))
                     ("required" . ("pattern")))
     :read-only-p t))

  ;; Grep tool
  (tool-registry-register registry
    (make-function-tool
     "Grep"
     "Search for a pattern in files"
     (lambda (arguments context)
       (let ((pattern (gethash "pattern" arguments "")))
         (handler-case
             (let ((output (with-output-to-string (s)
                             (run-program "grep" (list "-r" pattern ".")
                                          :output s
                                          :error-output s))))
               (make-tool-result :output output))
           (error (e)
             (create-tool-result (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("pattern" . (("type" . "string")
                                                    ("description" . "The regular expression pattern to search for")))))
                     ("required" . ("pattern")))
     :read-only-p t))

  registry)
