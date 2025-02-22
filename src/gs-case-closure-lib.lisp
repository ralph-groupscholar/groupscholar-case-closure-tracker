(require :asdf)
(require :uiop)

(defpackage :gs-case-closure
  (:use :cl :uiop))

(in-package :gs-case-closure)

(defun usage ()
  (format t "~%groupscholar-case-closure-tracker~%~%")
  (format t "Commands:~%")
  (format t "  init-db [--seed]                 Create schema and tables (and optionally seed).~%")
  (format t "  add --case-id ID --scholar NAME --program NAME --status STATUS \
[--closed-on YYYY-MM-DD] --owner NAME [--summary TEXT] [--next-step TEXT] [--risk-level LEVEL]~%")
  (format t "  list [--status STATUS] [--owner NAME] [--risk-level LEVEL] [--limit N]~%")
  (format t "  summary                           Show counts by status and risk level.~%")
  (format t "  help                              Show this message.~%")
  (format t "~%Environment:~%  DATABASE_URL must be set for DB actions.~%"))

(defun parse-args (args)
  (let ((result '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string-prefix-p "--" arg)
           (let ((key (subseq arg 2)))
             (if (null args)
                 (push (cons key t) result)
                 (let ((val (pop args)))
                   (push (cons key val) result)))))
          (t (push (cons "_cmd" arg) result)))))
    (nreverse result)))

(defun opt (opts key &optional default)
  (let ((entry (assoc key opts :test #'string=)))
    (if entry (cdr entry) default)))

(defun require-opt (opts key)
  (let ((val (opt opts key nil)))
    (unless val
      (format t "Missing required option: --~A~%" key)
      (usage)
      (uiop:quit 1))
    val))

(defun sql-escape (s)
  (if (null s)
      ""
      (with-output-to-string (out)
        (loop for ch across s do
          (if (char= ch #\')
              (write-string "''" out)
              (write-char ch out))))))

(defun sql-quote (s)
  (if (or (null s) (string= s ""))
      "NULL"
      (format nil "'~A'" (sql-escape s))))

(defun sql-date (s)
  (if (or (null s) (string= s ""))
      "NULL"
      (format nil "'~A'::date" (sql-escape s))))

(defun database-url ()
  (uiop:getenv "DATABASE_URL"))

(defun ensure-database-url ()
  (unless (database-url)
    (format t "DATABASE_URL is not set.~%")
    (uiop:quit 1)))

(defun run-psql (&rest args)
  (ensure-database-url)
  (let* ((cmd (append (list "psql" (database-url) "-v" "ON_ERROR_STOP=1" "-X") args))
         (proc (run-program cmd :output :string :error-output :string :ignore-error-status t)))
    (when (/= 0 (process-info-exit-code proc))
      (format t "psql error: ~A~%" (process-info-error-output proc))
      (uiop:quit 1))
    (process-info-output proc)))

(defun project-root ()
  (let* ((path (or *load-pathname* *load-truename* (uiop:getcwd))))
    (truename (merge-pathnames "../" path))))

(defun db-file (name)
  (merge-pathnames (format nil "db/~A" name) (project-root)))

(defun cmd-init-db (opts)
  (run-psql "-f" (namestring (db-file "init.sql")))
  (when (opt opts "seed" nil)
    (run-psql "-f" (namestring (db-file "seed.sql"))))
  (format t "Database initialized.~%"))

(defun build-where (opts)
  (let ((clauses '()))
    (when (opt opts "status" nil)
      (push (format nil "status = ~A" (sql-quote (opt opts "status"))) clauses))
    (when (opt opts "owner" nil)
      (push (format nil "owner = ~A" (sql-quote (opt opts "owner"))) clauses))
    (when (opt opts "risk-level" nil)
      (push (format nil "risk_level = ~A" (sql-quote (opt opts "risk-level"))) clauses))
    (if clauses
        (format nil "WHERE ~{~A~^ AND ~}" (nreverse clauses))
        "")))

(defun cmd-add (opts)
  (let* ((case-id (require-opt opts "case-id"))
         (scholar (require-opt opts "scholar"))
         (program (require-opt opts "program"))
         (status (require-opt opts "status"))
         (closed-on (opt opts "closed-on" nil))
         (owner (require-opt opts "owner"))
         (summary (opt opts "summary" nil))
         (next-step (opt opts "next-step" nil))
         (risk-level (opt opts "risk-level" "medium"))
         (sql (format nil
                      "INSERT INTO groupscholar_case_closure.case_closures\
(case_id, scholar, program, status, closed_on, owner, summary, next_step, risk_level)\
VALUES (~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A, ~A)\
RETURNING id;"
                      (sql-quote case-id)
                      (sql-quote scholar)
                      (sql-quote program)
                      (sql-quote status)
                      (sql-date closed-on)
                      (sql-quote owner)
                      (sql-quote summary)
                      (sql-quote next-step)
                      (sql-quote risk-level))))
    (let ((out (run-psql "-t" "-A" "-c" sql)))
      (format t "Added case closure with id ~A~%" (string-trim '(#\Space #\Newline #\Tab) out)))))

(defun format-rows (rows)
  (format t "id | case_id | scholar | program | status | closed_on | owner | risk_level~%")
  (format t "-~%")
  (dolist (row rows)
    (format t "~A~%" (substitute #\| #\Tab row))))

(defun cmd-list (opts)
  (let* ((where-clause (build-where opts))
         (limit (opt opts "limit" "50"))
         (sql (format nil
                      "SELECT id, case_id, scholar, program, status, COALESCE(closed_on::text, ''), owner, risk_level\
FROM groupscholar_case_closure.case_closures\
~A\
ORDER BY created_at DESC\
LIMIT ~A;"
                      where-clause
                      (sql-escape limit)))
         (out (run-psql "-t" "-A" "-F" (string #\Tab) "-c" sql))
         (lines (remove-if #'(lambda (s) (string= s ""))
                           (split-string out :separator #\Newline))))
    (format-rows lines)))

(defun cmd-summary ()
  (let* ((sql "SELECT status, risk_level, COUNT(*)\
FROM groupscholar_case_closure.case_closures\
GROUP BY status, risk_level\
ORDER BY status, risk_level;")
         (out (run-psql "-t" "-A" "-F" (string #\Tab) "-c" sql))
         (lines (remove-if #'(lambda (s) (string= s ""))
                           (split-string out :separator #\Newline))))
    (format t "status | risk_level | count~%")
    (format t "-~%")
    (dolist (row lines)
      (format t "~A~%" (substitute #\| #\Tab row)))))

(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (cmd (if args (first args) "help"))
         (opts (parse-args (rest args))))
    (cond
      ((or (string= cmd "help") (string= cmd "-h") (string= cmd "--help"))
       (usage))
      ((string= cmd "init-db")
       (cmd-init-db opts))
      ((string= cmd "add")
       (cmd-add opts))
      ((string= cmd "list")
       (cmd-list opts))
      ((string= cmd "summary")
       (cmd-summary))
      (t
       (format t "Unknown command: ~A~%" cmd)
       (usage)
       (uiop:quit 1)))))
