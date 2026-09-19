;;;; -- Shell execution context --

(in-package #:cclsh)

(defvar *shell-input-fd* 0 "This shell's input descriptor.")
(defvar *shell-output-fd* 1 "This shell's output descriptor.")
(defvar *shell-error-fd* 2 "This shell's diagnostic descriptor.")
(defvar *shell-session* nil "Shared-server session, or NIL in an ordinary shell.")
(defvar *session-environment* nil "Session environment table, or NIL for libc.")
(defvar *session-process-function* nil "Session native-job protocol adapter.")
