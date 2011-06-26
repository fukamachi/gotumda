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
        :elephant))
(in-package :gotumda-test)

(plan nil)

(gotumda:stop)

(diag "Starting..")
(gotumda:start :mode "test")

(diag "all-tasks")
(is (flex:octets-to-string
     (http-request "http://localhost:4242/api/all-tasks"))
    "[]"
    "all-tasks (api)")

(diag "update")
(http-request "http://localhost:4242/update"
                  :method :POST
                  :parameters '(("body" . "Buy a milk")))

(diag "all-tasks 2")
(like (flex:octets-to-string
       (http-request "http://localhost:4242/api/all-tasks"))
      "Buy a milk"
      "all-tasks (api)")

(diag "Stopping..")

(drop-instances
 (get-instances-by-class 'gotumda.model:<task>))

(gotumda:stop)

(finalize)
