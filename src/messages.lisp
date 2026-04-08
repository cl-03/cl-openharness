;;; messages.lisp - Conversation message structures
;;;
;;; This file defines the message types used for agent conversations,
;;; including text blocks, tool use blocks, and tool result blocks.

(in-package #:cl-openharness)

;;; ============================================================================
;;; Text Block
;;; ============================================================================

(defstruct text-block
  "Plain text content block."
  (type "text" :read-only t)
  (text "" :type string))

;;; ============================================================================
;;; Tool Use Block
;;; ============================================================================

(defstruct tool-use-block
  "A request from the model to execute a named tool."
  (type "tool_use" :read-only t)
  (id (generate-uuid) :type string)
  (name "" :type string)
  (input (make-hash-table :test 'equal) :type hash-table))

;;; ============================================================================
;;; Tool Result Block
;;; ============================================================================

(defstruct tool-result-block
  "Tool result content sent back to the model."
  (type "tool_result" :read-only t)
  (tool-use-id "" :type string)
  (content "" :type string)
  (is-error nil :type boolean))

;;; ============================================================================
;;; Content Block Union
;;; ============================================================================

(defgeneric content-block-type (block)
  (:method (block)
    nil)
  (:method ((block text-block))
    "text")
  (:method ((block tool-use-block))
    "tool_use")
  (:method ((block tool-result-block))
    "tool_result"))

;;; ============================================================================
;;; Conversation Message
;;; ============================================================================

(defstruct conversation-message
  "A single assistant or user message."
  (role "user" :type string)
  (content nil :type list))

(defun conversation-message-from-user-text (text)
  "Construct a user message from raw text."
  (make-conversation-message :role "user" :content (list (make-text-block :text text))))

(defun conversation-message-text (message)
  "Return concatenated text blocks from a message."
  (with-output-to-string (stream)
    (dolist (block (conversation-message-content message))
      (when (text-block-p block)
        (write-string (text-block-text block) stream)))))

(defun conversation-message-tool-uses (message)
  "Return all tool use blocks contained in the message."
  (remove-if-not #'tool-use-block-p (conversation-message-content message)))

(defun conversation-message-tool-results (message)
  "Return all tool result blocks contained in the message."
  (remove-if-not #'tool-result-block-p (conversation-message-content message)))

;;; ============================================================================
;;; Serialization for API
;;; ============================================================================

(defun serialize-content-block (block)
  "Convert a content block into the provider wire format."
  (etypecase block
    (text-block
     `(("type" . "text")
       ("text" . ,(text-block-text block))))
    (tool-use-block
     `(("type" . "tool_use")
       ("id" . ,(tool-use-block-id block))
       ("name" . ,(tool-use-block-name block))
       ("input" . ,(hash-table-to-alist (tool-use-block-input block)))))
    (tool-result-block
     `(("type" . "tool_result")
       ("tool_use_id" . ,(tool-result-block-tool-use-id block))
       ("content" . ,(tool-result-block-content block))
       ("is_error" . ,(tool-result-block-is-error block))))))

(defun message-to-api-param (message)
  "Convert a conversation message into API params."
  `(("role" . ,(conversation-message-role message))
    ("content" . ,(mapcar #'serialize-content-block
                          (conversation-message-content message)))))

(defun hash-table-to-alist (ht)
  "Convert a hash table to an alist."
  (let (alist)
    (maphash (lambda (k v)
               (push (cons k v) alist))
             ht)
    (nreverse alist)))

(defun alist-to-hash-table (alist)
  "Convert an alist to a hash table."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

;;; ============================================================================
;;; Deserialization from API
;;; ============================================================================

(defun assistant-message-from-api (raw-message)
  "Convert an API message object into a conversation message."
  (let ((content nil))
    (when (and (alist-p raw-message)
               (assoc "content" raw-message :test 'string=))
      (dolist (raw-block (cdr (assoc "content" raw-message :test 'string=)))
        (let ((block-type (cdr (assoc "type" raw-block :test 'string=))))
          (cond
            ((string= block-type "text")
             (push (make-text-block
                    :text (cdr (assoc "text" raw-block :test 'string=)))
                   content))
            ((string= block-type "tool_use")
             (push (make-tool-use-block
                    :name (cdr (assoc "name" raw-block :test 'string=))
                    :input (alist-to-hash-table
                            (cdr (assoc "input" raw-block :test 'string=)))
                    :id (or (cdr (assoc "id" raw-block :test 'string=))
                            (generate-uuid)))
                   content))))))
    (make-conversation-message :role "assistant" :content (nreverse content))))

(defun alist-p (obj)
  "Check if an object is an alist."
  (and (listp obj)
       (every (lambda (x)
                (and (consp x)
                     (or (symbolp (car x))
                         (stringp (car x)))))
              obj)))

;;; ============================================================================
;;; Message Helpers
;;; ============================================================================

(defun has-pending-continuation-p (messages)
  "Check if conversation ends with tool results awaiting follow-up."
  (when messages
    (let ((last (car (last messages))))
      (when (and (conversation-message-p last)
                 (string= (conversation-message-role last) "user"))
        (when (some #'tool-result-block-p
                    (conversation-message-content last))
          ;; Check if there's an assistant message with tool_uses before
          (dolist (msg (reverse (butlast messages)))
            (when (string= (conversation-message-role msg) "assistant")
              (return-from has-pending-continuation-p
                (conversation-message-tool-uses msg)))))))))

(defun messages-to-api-params (messages)
  "Convert a list of messages to API params."
  (mapcar #'message-to-api-param messages))
