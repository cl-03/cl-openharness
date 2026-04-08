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
       (let ((pattern (gethash "pattern" arguments ""))
             (path (gethash "path" arguments ".")))
         (handler-case
             (let ((output (with-output-to-string (s)
                             (run-program "grep" (list "-r" pattern path)
                                          :output s
                                          :error-output s))))
               (make-tool-result :output output))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("pattern" . (("type" . "string")
                                                    ("description" . "The regular expression pattern to search for")))
                                      ("path" . (("type" . "string")
                                                 ("description" . "The directory to search in (default: current directory)")))))
                     ("required" . ("pattern")))
     :read-only-p t))

  ;; Delete tool
  (tool-registry-register registry
    (make-function-tool
     "Delete"
     "Delete a file or directory"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments ""))
             (recursive (gethash "recursive" arguments nil)))
         (handler-case
             (progn
               (if recursive
                   (sb-ext:run-program "rm" (list "-rf" file-path))
                   (delete-file file-path))
               (make-tool-result :output (format nil "Successfully deleted ~a" file-path)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file or directory to delete")))
                                      ("recursive" . (("type" . "boolean")
                                                      ("description" . "Whether to recursively delete directories (default: false)")))))
                     ("required" . ("file_path")))))

  ;; Copy tool
  (tool-registry-register registry
    (make-function-tool
     "Copy"
     "Copy a file or directory"
     (lambda (arguments context)
       (let ((source (gethash "source" arguments ""))
             (destination (gethash "destination" arguments ""))
             (recursive (gethash "recursive" arguments nil)))
         (handler-case
             (progn
               (if recursive
                   (sb-ext:run-program "cp" (list "-r" source destination))
                   (sb-ext:run-program "cp" (list source destination)))
               (make-tool-result :output (format nil "Successfully copied ~a to ~a" source destination)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("source" . (("type" . "string")
                                                   ("description" . "The source file or directory path")))
                                      ("destination" . (("type" . "string")
                                                        ("description" . "The destination path")))
                                      ("recursive" . (("type" . "boolean")
                                                      ("description" . "Whether to recursively copy directories (default: false)")))))
                     ("required" . ("source" "destination")))))

  ;; Move tool
  (tool-registry-register registry
    (make-function-tool
     "Move"
     "Move or rename a file or directory"
     (lambda (arguments context)
       (let ((source (gethash "source" arguments ""))
             (destination (gethash "destination" arguments "")))
         (handler-case
             (progn
               (sb-ext:run-program "mv" (list source destination))
               (make-tool-result :output (format nil "Successfully moved ~a to ~a" source destination)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("source" . (("type" . "string")
                                                   ("description" . "The source file or directory path")))
                                      ("destination" . (("type" . "string")
                                                        ("description" . "The destination path")))))
                     ("required" . ("source" "destination")))))

  ;; ListDirectory tool
  (tool-registry-register registry
    (make-function-tool
     "ListDirectory"
     "List contents of a directory"
     (lambda (arguments context)
       (let ((dir-path (gethash "dir_path" arguments "."))
             (show-hidden (gethash "show_hidden" arguments nil)))
         (handler-case
             (let* ((files (directory (merge-pathnames "*#" (pathname dir-path))))
                    (filtered (if show-hidden
                                  files
                                  (remove-if (lambda (f)
                                               (char= #\. (char (namestring f) 0)))
                                             files))))
               (make-tool-result :output (format nil "~{~a~%~}" (mapcar #'namestring filtered))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("dir_path" . (("type" . "string")
                                                     ("description" . "The directory path to list (default: current directory)")))
                                      ("show_hidden" . (("type" . "boolean")
                                                        ("description" . "Whether to show hidden files (default: false)")))))
                     ("required" . ("dir_path")))
     :read-only-p t))

  ;; Touch tool
  (tool-registry-register registry
    (make-function-tool
     "Touch"
     "Create an empty file or update timestamp"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments "")))
         (handler-case
             (progn
               (with-open-file (stream file-path :direction :output
                                                :if-exists :overwrite
                                                :if-does-not-exist :create)
                 (setf (file-length stream) 0))
               (make-tool-result :output (format nil "Successfully touched ~a" file-path)))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The file path to touch")))))
                     ("required" . ("file_path")))))

  ;; Cat tool (Read with line numbers)
  (tool-registry-register registry
    (make-function-tool
     "Cat"
     "Read a file with line numbers"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments "")))
         (handler-case
             (with-open-file (stream file-path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 (let ((lines (split-sequence:split-sequence #\Newline content))
                       (numbered (with-output-to-string (s)
                                   (loop for line in lines
                                         for i from 1
                                         do (format s "~5d: ~a~%" i line)))))
                   (make-tool-result :output numbered))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to read")))))
                     ("required" . ("file_path")))
     :read-only-p t))

  ;; Head tool
  (tool-registry-register registry
    (make-function-tool
     "Head"
     "Read the first N lines of a file"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments ""))
             (lines (gethash "lines" arguments 10)))
         (handler-case
             (with-open-file (stream file-path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 (let* ((all-lines (split-sequence:split-sequence #\Newline content))
                        (head-lines (subseq all-lines 0 (min lines (length all-lines)))))
                   (make-tool-result :output (format nil "~{~a~%~}" head-lines)))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to read")))
                                      ("lines" . (("type" . "integer")
                                                  ("description" . "Number of lines to read (default: 10)")))))
                     ("required" . ("file_path")))
     :read-only-p t))

  ;; Tail tool
  (tool-registry-register registry
    (make-function-tool
     "Tail"
     "Read the last N lines of a file"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments ""))
             (lines (gethash "lines" arguments 10)))
         (handler-case
             (with-open-file (stream file-path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 (let* ((all-lines (split-sequence:split-sequence #\Newline content))
                        (tail-start (max 0 (- (length all-lines) lines)))
                        (tail-lines (subseq all-lines tail-start)))
                   (make-tool-result :output (format nil "~{~a~%~}" tail-lines)))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to read")))
                                      ("lines" . (("type" . "integer")
                                                  ("description" . "Number of lines to read from the end (default: 10)")))))
                     ("required" . ("file_path")))
     :read-only-p t))

  ;; Wc tool (Word Count)
  (tool-registry-register registry
    (make-function-tool
     "Wc"
     "Count lines, words, and characters in a file"
     (lambda (arguments context)
       (let ((file-path (gethash "file_path" arguments "")))
         (handler-case
             (with-open-file (stream file-path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 (let ((lines (length (split-sequence:split-sequence #\Newline content)))
                       (words (length (split-sequence:split-sequence #\Space content)))
                       (chars (length content)))
                   (make-tool-result :output (format nil "~a lines, ~a words, ~a characters~%" lines words chars)))))
           (error (e)
             (make-tool-result :output (format nil "Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("file_path" . (("type" . "string")
                                                      ("description" . "The absolute path to the file to count")))))
                     ("required" . ("file_path")))
     :read-only-p t))

  ;; JsonParse tool
  (tool-registry-register registry
    (make-function-tool
     "JsonParse"
     "Parse a JSON string and return formatted output"
     (lambda (arguments context)
       (let ((json-string (gethash "json_string" arguments ""))
             (pretty (gethash "pretty" arguments t)))
         (handler-case
             (let ((parsed (jonathan:parse json-string)))
               (if pretty
                   (make-tool-result :output (jonathan:to-json parsed))
                   (make-tool-result :output (format nil "~a" parsed))))
           (error (e)
             (make-tool-result :output (format nil "JSON Parse Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("json_string" . (("type" . "string")
                                                        ("description" . "The JSON string to parse")))
                                      ("pretty" . (("type" . "boolean")
                                                   ("description" . "Whether to format output (default: true)")))))
                     ("required" . ("json_string")))
     :read-only-p t))

  ;; HttpRequest tool
  (tool-registry-register registry
    (make-function-tool
     "HttpRequest"
     "Send an HTTP request and return the response"
     (lambda (arguments context)
       (let ((url (gethash "url" arguments ""))
             (method (gethash "method" arguments "GET"))
             (headers (gethash "headers" arguments nil))
             (body (gethash "body" arguments nil)))
         (handler-case
             (let ((response (cond
                               ((string= method "GET")
                                (dex:get url))
                               ((string= method "POST")
                                (dex:post url :content body))
                               ((string= method "PUT")
                                (dex:put url :content body))
                               ((string= method "DELETE")
                                (dex:delete url))
                               (t (dex:get url)))))
               (make-tool-result :output (format nil "Status: 200~%~a" response)))
           (error (e)
             (make-tool-result :output (format nil "HTTP Error: ~a" e) :is-error t)))))
     :input-schema '(("type" . "object")
                     ("properties" . (("url" . (("type" . "string")
                                                ("description" . "The URL to request")))
                                      ("method" . (("type" . "string")
                                                   ("description" . "HTTP method: GET, POST, PUT, DELETE (default: GET)")))
                                      ("headers" . (("type" . "object")
                                                    ("description" . "HTTP headers as key-value pairs")))
                                      ("body" . (("type" . "string")
                                                 ("description" . "Request body for POST/PUT requests")))))
                     ("required" . ("url")))))

  registry)
