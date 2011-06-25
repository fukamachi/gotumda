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
        :cl-test-more))
(in-package :gotumda-test)

(plan nil)

(gotumda:stop)

(diag "Starting..")
(gotumda:start :mode "test")

(diag "index")
(let ((index-page (http-request "http://localhost:4242/")))
  (like index-page "Welcome to Gotumda!"
        "index")
  (is (http-request "http://localhost:4242/pc/")
      index-page
      "index (pc)"))

(diag "tasks")
(let ((tasks-page (http-request "http://localhost:4242/tasks")))
  (is tasks-page "" "tasks")
  (is (http-request "http://localhost:4242/pc/tasks")
      tasks-page
      "tasks (pc)"))
(is (flex:octets-to-string
     (http-request "http://localhost:4242/api/tasks"))
    "[]"
    "tasks (api)")

(diag "update")
(http-request "http://localhost:4242/update"
              :method :POST
              :parameters '(("body" . "Buy a milk")))

(diag "tasks 2")
(let ((tasks-page (http-request "http://localhost:4242/tasks")))
  (is tasks-page "" "tasks")
  (is (http-request "http://localhost:4242/pc/tasks")
      tasks-page
      "tasks (pc)"))
(is (flex:octets-to-string
     (http-request "http://localhost:4242/api/tasks"))
    "[]"
    "tasks (api)")

(diag "Stopping..")

(map nil
     #'elephant:drop-instance
     (elephant:get-instances-by-class 'gotumda.model:<task>))

(gotumda:stop)

(finalize)
