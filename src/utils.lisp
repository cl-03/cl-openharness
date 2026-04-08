;;; utils.lisp - Utility functions and helpers
;;;
;;; This file provides common utilities used throughout cl-openharness.

(in-package #:cl-openharness)

;;; ============================================================================
;;; UUID Generation
;;; ============================================================================

(defun generate-uuid ()
  "Generate a UUID string."
  (flet ((hex-digit ()
           (char "0123456789abcdef" (random 16))))
    (format nil "toolu_~{~a~}"
            (loop repeat 32 collect (hex-digit)))))

;;; ============================================================================
;;; JSON Utilities
;;; ============================================================================

(defun json-encode (object)
  "Encode an object to JSON string."
  (jonathan:json object))

(defun json-decode (string)
  "Decode a JSON string to an object."
  (jonathan:json string :json-keyword nil))

;;; ============================================================================
;;; Path Utilities
;;; ============================================================================

(defun normalize-path (path)
  "Normalize a file path to absolute form."
  (uiop:native-namestring
   (uiop:ensure-absolute-pathname
    (uiop:parse-unix-namestring path))))

(defun match-path-pattern (path pattern)
  "Match a path against a glob pattern."
  (let ((path-parts (split-sequence:split-sequence #\/ path))
        (pattern-parts (split-sequence:split-sequence #\/ pattern)))
    (labels ((match-part (path-part pattern-part)
               (cond
                 ((string= pattern-part "*") t)
                 ((string= pattern-part "**") t)
                 ((string= path-part pattern-part) t)
                 ((and (string= pattern-part "*.*")
                       (find #\. path-part)) t)
                 (t (cl-mismatch path-part pattern-part)))))
      (if (= (length path-parts) (length pattern-parts))
          (every #'match-part path-parts pattern-parts)
          nil))))

;;; ============================================================================
;;; Retry Utilities
;;; ============================================================================

(defparameter *max-retries* 3
  "Maximum number of retry attempts for API calls.")

(defparameter *base-delay* 1.0
  "Base delay in seconds for exponential backoff.")

(defparameter *max-delay* 30.0
  "Maximum delay in seconds for retries.")

(defparameter *retryable-status-codes*
  '(429 500 502 503 529)
  "HTTP status codes that should trigger a retry.")

(defun calculate-retry-delay (attempt &optional (base *base-delay*) (max *max-delay*))
  "Calculate delay with exponential backoff and jitter."
  (let* ((exponential-delay (* base (expt 2 attempt)))
         (capped-delay (min exponential-delay max))
         (jitter (* capped-delay 0.25 (random 1.0))))
    (+ capped-delay jitter)))

(defun with-retry ((&key (max-attempts *max-retries*)
                         (delay-fn #'calculate-retry-delay)
                         (retryable-p (lambda (condition)
                                       (declare (ignore condition))
                                       t))
                         (on-retry (lambda (condition attempt)
                                    (declare (ignore condition attempt))
                                    nil)))
  "Execute body with retry logic."
  (let ((attempt 0))
    (loop
      (handler-case
          (return (progn ,@body))
        (condition (c)
          (incf attempt)
          (if (>= attempt max-attempts)
              (error c)
              (when (funcall retryable-p c)
                (funcall on-retry c attempt)
                (sleep (funcall delay-fn (1- attempt))))))))))

;;; ============================================================================
;;; Async Utilities (using bordeaux-threads)
;;; ============================================================================

(defmacro with-timeout ((timeout-seconds) &body body)
  "Execute body with a timeout."
  `(let ((thread (bordeaux-threads:make-thread
                  (lambda ()
                    (let ((result (progn ,@body)))
                      (bordeaux-threads:make-instance
                       'thread-result
                       :success t
                       :value result))))))
     (if (bordeaux-threads:join-thread thread :timeout ,timeout-seconds)
         (thread-result-value (bordeaux-threads:thread-result thread))
         (progn
           (bordeaux-threads:destroy-thread thread)
           (error "Operation timed out after ~a seconds" ,timeout-seconds)))))

(defstruct thread-result
  success
  value)

;;; ============================================================================
;;; Logging
;;; ============================================================================

(defparameter *log-level* :info
  "Current logging level.")

(defparameter *log-output* *standard-output*
  "Stream for log output.")

(defun log (level format-string &rest args)
  "Log a message at the specified level."
  (when (or (eq level :debug)
            (and (eq level :info) (member *log-level* '(:info :warning :error)))
            (and (eq level :warning) (member *log-level* '(:warning :error)))
            (eq level :error))
    (apply #'format *log-output*
           (format nil "[~a] ~a: "
                   (get-universal-time)
                   (string-upcase (string level)))
           format-string args)
    (terpri *log-output*)))

(defmacro defun-logging (name args &body body)
  "Define a function with automatic entry/exit logging."
  `(defun ,name ,args
     (log :debug "Entering ~a with args: ~s" ',name (list ,@args))
     (let ((result (progn ,@body)))
       (log :debug "Exiting ~a with result: ~s" ',name result)
       result)))
