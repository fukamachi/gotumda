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

(diag "all-tasks")
(is (flex:octets-to-string
     (http-request "http://localhost:4242/api/all-tasks"))
    "[]"
    "all-tasks (api)")

(diag "new")
(let ((task
       (with-input-from-string
           (s (flex:octets-to-string
               (http-request "http://localhost:4242/api/update"
                :method :POST
                :parameters '(("body" . "Buy a milk")))))
         (json:decode-json s))))
  (is (cdr (assoc :body task))
      "Buy a milk")

  (diag "all-tasks 2")
  (is (flex:octets-to-string
       (http-request "http://localhost:4242/api/all-tasks"))
      (format nil
              "[{\"id\":\"~A\",\"body\":\"Buy a milk\"}]"
              (cdr (assoc :id task)))
      "all-tasks (api)")

  (diag "update")
  (is (flex:octets-to-string
       (http-request "http://localhost:4242/api/update"
        :method :POST
        :parameters `(("id" . ,(cdr (assoc :id task)))
                      ("body" . "Buy eggs"))))
      (format nil
              "{\"id\":\"~A\",\"body\":\"Buy eggs\"}"
              (cdr (assoc :id task)))))

(diag "Stopping..")

(drop-instances
 (get-instances-by-class 'gotumda.model:<task>))

(gotumda:stop)

(finalize)
