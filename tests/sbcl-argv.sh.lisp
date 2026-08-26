;; Fixture for scripts/sbcl-check.  Script mode must retain dash-prefixed data.
(progn
  (format t "__SBCL_ARGV__~s__~%" *argv*)
  (values))
