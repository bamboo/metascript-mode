
(defmacro with-metascript-buffer (&rest body)
  "Similar to `with-temp-buffer' except the body is evaluated after `metascript-mode'."
  (declare (indent 0))
  `(with-temp-buffer
     (let ((metascript-mode-hook nil))
       (metascript-mode)
       ,@body)))


(defmacro let-temp-buffer (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let ((,symbol (generate-new-buffer " *temp*")))
     (unwind-protect
         (progn ,@body)
       (and (buffer-name ,symbol)
            (kill-buffer ,symbol)))))


(defun metascript-accept-repl-output (repl)
  (accept-process-output (get-buffer-process repl) 1))


(defmacro let-temp-metascript-repl (symbol &rest body)
  "Create a temporary buffer, assign it to symbol, evaluate BODY like `progn', and then kill the buffer."
  (declare (indent 1) (debug t))
  `(let* ((,symbol (metascript-repl-make-comint))
	  (repl-process (get-buffer-process ,symbol)))
     (unwind-protect
         (progn
           (set-process-query-on-exit-flag repl-process nil)
           (metascript-accept-repl-output ,symbol)
           ,@body)
       (progn
	 (when (process-live-p repl-process)
	   (quit-process repl-process))
         (and (buffer-name ,symbol)
              (kill-buffer ,symbol))))))


(ert-deftest metascript-mode-test/can-mark-sexp ()
  (with-sandbox
   (with-metascript-buffer
     (let ((metascript-sexp "var f = () ->\n  42\n"))
       (insert metascript-sexp metascript-sexp)
       (goto-char (point-min))
       (metascript-mark-sexp)
       (should (equal metascript-sexp (metascript-region-string)))))))


(defun test-metascript-repl-eval-after (setup)
  (with-sandbox
   (let-temp-metascript-repl repl
    (with-metascript-buffer
      (insert "var a = 2 * 21")
      (funcall setup)
      (metascript-repl-eval repl)
      (metascript-accept-repl-output repl)
      (with-current-buffer repl
	(let ((expected "mjs> 42\n"))
	  (should (equal expected (buffer-substring-no-properties (point-min) (+ (point-min) (length expected)))))))))))


(ert-deftest metascript-mode-test/repl-eval-sends-active-region-to-repl ()
  (test-metascript-repl-eval-after
   (lambda () (set-mark 9))))


(ert-deftest metascript-mode-test/repl-eval-sends-enclosing-sexp-when-region-is-not-active ()
  (test-metascript-repl-eval-after
   (lambda () (goto-char (point-max)))))
