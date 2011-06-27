#|
  This file is a part of Gotumda project.
|#

(in-package :cl-user)
(defpackage gotumda-test
  (:use :cl
        :gotumda
        :drakma
        :flexi-streams
        :cl-fad
        :cl-test-more
        :elephant
        :json))
(in-package :gotumda-test)

(plan nil)

(defun request-json (url &rest args)
  "HTTP request to the url and return the result as a decoded JSON.
Note the url doesn't contain http://localhost:4242/.

Example:
  (request-json \"api/update\"
                :method :POST
                :parameters '((\"body\" . \"Buy a milk\")))
"
  (with-input-from-string
      (s (flex:octets-to-string
          (apply #'http-request
           (concatenate 'string "http://localhost:4242/" url)
           args)))
    (json:decode-json s)))

(gotumda:stop)

(diag "Starting..")
(gotumda:start :mode "test")

;; cleanup database
(drop-instances
 (get-instances-by-class 'gotumda.model:<task>))

(is (request-json "api/all-tasks.json")
    '()
    "no task")

(defvar task nil)
(setf task
      (request-json "api/update.json"
                    :method :POST
                    :parameters '(("body" . "Buy a milk"))))

(is (cdr (assoc :body task))
    "Buy a milk"
    "create new task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy a milk")
       (:is-done . "false")))
    "one task")

(is (request-json "api/update.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))
                                ("body" . "Buy eggs")))
    `((:id . ,(cdr (assoc :id task)))
      (:body . "Buy eggs")
      (:is-done . "false"))
    "edit the task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy eggs")
       (:is-done . "false")))
    "one task")

(is (request-json "api/update.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))
                                ("isDone" . "true")))
    `((:id . ,(cdr (assoc :id task)))
      (:body . "Buy eggs")
      (:is-done . "true"))
    "done the task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy eggs")
       (:is-done . "true")))
    "one done task")

(ok (request-json "api/destroy.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))))
    "delete the task")

(is (request-json "api/all-tasks.json")
    '()
    "no task")

(diag "Stopping..")

(gotumda:stop)

(finalize)
