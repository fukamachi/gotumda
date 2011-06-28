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
(defvar task2 nil)
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
       (:url . "")
       (:is-done . "false")))
    "one task")

(setf task2
      (request-json "api/update.json"
                    :method :POST
                    :parameters '(("body" . "Clean the kitchen"))))

(is (cdr (assoc :body task2))
    "Clean the kitchen"
    "create another task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy a milk")
       (:url . "")
       (:is-done . "false"))
      ((:id . ,(cdr (assoc :id task2)))
       (:body . "Clean the kitchen")
       (:url . "")
       (:is-done . "false")))
    "two tasks")

(is (request-json "api/update.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))
                                ("body" . "Buy eggs")))
    `((:id . ,(cdr (assoc :id task)))
      (:body . "Buy eggs")
      (:url . "")
      (:is-done . "false"))
    "edit the task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task)))
       (:body . "Buy eggs")
       (:url . "")
       (:is-done . "false"))
      ((:id . ,(cdr (assoc :id task2)))
       (:body . "Clean the kitchen")
       (:url . "")
       (:is-done . "false")))
    "two tasks")

(is (request-json "api/sort-tasks.json"
                  :method :POST
                  :parameters
                  `(("order" . ,(format nil
                                        "~A,~A"
                                        (cdr (assoc :id task2))
                                        (cdr (assoc :id task))))))
    (list
     (parse-integer (cdr (assoc :id task2)))
     (parse-integer (cdr (assoc :id task))))
    "sort tasks")

(is (request-json "api/update.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))
                                ("isDone" . "true")))
    `((:id . ,(cdr (assoc :id task)))
      (:body . "Buy eggs")
      (:url . "")
      (:is-done . "true"))
    "done a task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task2)))
       (:body . "Clean the kitchen")
       (:url . "")
       (:is-done . "false"))
      ((:id . ,(cdr (assoc :id task)))
       (:body . "Buy eggs")
       (:url . "")
       (:is-done . "true")))
    "one done task")

(ok (request-json "api/destroy.json"
                  :method :POST
                  :parameters `(("id" . ,(cdr (assoc :id task)))))
    "delete the task")

(is (request-json "api/all-tasks.json")
    `(((:id . ,(cdr (assoc :id task2)))
       (:body . "Clean the kitchen")
       (:url . "")
       (:is-done . "false")))
    "one task")

(diag "Stopping..")

(gotumda:stop)

(finalize)
