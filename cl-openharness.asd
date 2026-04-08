;;; cl-openharness.asd - OpenHarness Common Lisp Implementation
;;;
;;; An open agent harness for building AI agents with tools, skills, memory,
;;; and multi-agent coordination.

(asdf:defsystem #:cl-openharness
  :version "0.1.0"
  :author "OpenHarness Community"
  :description "Open Agent Harness - Core infrastructure for AI agents"
  :license "MIT"
  :depends-on (#:dexador
               #:jonathan
               #:closer-mop
               #:bordeaux-threads
               #:uiop)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "utils")
               (:file "messages")
               (:file "tools")
               (:file "permissions")
               (:file "hooks")
               (:file "api-client")
               (:file "query-engine")
               (:file "runtime")))

(asdf:defsystem #:cl-openharness/test
  :version "0.1.0"
  :description "Tests for cl-openharness"
  :license "MIT"
  :depends-on (#:cl-openharness #:paradox)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "test-messages")
               (:file "test-tools")
               (:file "test-permissions")
               (:file "run-tests")))
