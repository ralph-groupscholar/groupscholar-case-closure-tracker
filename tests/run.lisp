(load (merge-pathnames "../src/gs-case-closure-lib.lisp" *load-truename*))

(in-package :gs-case-closure)

(defun assert-equal (expected actual label)
  (unless (equal expected actual)
    (format t "FAIL: ~A expected ~A got ~A~%" label expected actual)
    (uiop:quit 1)))

(defun run-tests ()
  (assert-equal "O''Brien" (sql-escape "O'Brien") "sql-escape single quote")
  (assert-equal "NULL" (sql-quote "") "sql-quote empty")
  (assert-equal "'2026-02-01'::date" (sql-date "2026-02-01") "sql-date")
  (let ((opts (parse-args '("--status" "open" "--owner" "Jordan"))))
    (assert-equal "WHERE status = 'open' AND owner = 'Jordan'" (build-where opts) "build-where"))
  (format t "All tests passed.~%"))

(run-tests)
