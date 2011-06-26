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

(gotumda:stop)

(diag "Starting..")
(gotumda:start :mode "test")

(is (request-json "api/all-tasks")
    '()
    "no task")

(defvar task nil)
(setf task
      (request-json "api/update"
                    :method :POST
                    :parameters '(("body" . "Buy a milk"))))

(is (cdr (assoc :body task))
    "Buy a milk"
    "create new task")

(is (request-json "api/all-tasks")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy a milk")))
    "one task")

(is (request-json "api/update"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))
                                ("body" . "Buy eggs")))
    `((:id . ,(cdr (assoc :id task)))
      (:body . "Buy eggs"))
    "edit the task")

(is (request-json "api/all-tasks")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy eggs")))
    "one task")

(diag "Stopping..")

(drop-instances
 (get-instances-by-class 'gotumda.model:<task>))

(gotumda:stop)

(finalize)

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
