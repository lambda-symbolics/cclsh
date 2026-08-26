;;;; -- SBCL prewarm boundary --
;;;
;;; Prewarming relies on a saved CCL image.  The SBCL runner loads ordinary
;;; source in each process, so it deliberately has no daemon protocol.

(in-package #:cclsh)

(defparameter +prewarm-worker-fd-variable+ "CCLSH_FAST_WORKER_FD")
(defparameter +prewarm-worker-argument+ "--cclsh-fast-worker")

(defun prewarm--hard-exit ()
  "Terminate an impossible SBCL prewarm worker request."
  (quit 70))

(defun prewarm-worker-lease (descriptor)
  "Reject CCL-only prewarm leases in the SBCL source runner."
  (declare (ignore descriptor))
  (error "cclsh-fast requires a saved CCL image; use cclsh-sbcl instead"))
