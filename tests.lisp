;; super basic test function
(defun assert-value (test-expectation test-result &key description)
  "Checks if the expected result matches the test, if not sends a message."
  (if (not (equal test-expectation test-result))
      (progn (if description
                 (format t "FAILED TEST ~a, expected ~a got ~a~%" description test-expectation test-result)
                 (format t "FAILED TEST expected ~a got ~a~%" test-expectation test-result))
             nil)
      t))
(defun run-test-fun (test-fun &key desc)
  "Runs the test and print results"
  (if (funcall test-fun)
      (progn (if desc
                 (format t "TEST ~a PASSED~%" desc)
                 (format t "TEST ~a PASSED~%" test-fun))
             t)))
(defun run-test (test-fun &key description timed)
  "Wrap a test function and print time and success"
  (if timed
      (time (run-test-fun test-fun :desc description))
      (run-test-fun test-fun :desc description)))

;;; Tests for math.lisp
(load "math.lisp")

(defun test--process-math ()
  "Test the process-math function"
  (and (assert-value 40 (process-math "2 * 100 / (3 + 2)"))
       t))

(defun test-all (&key timed)
  "Run all tests, if timed will run each timed."
  (time (progn (run-test 'test--process-math :timed timed))))
