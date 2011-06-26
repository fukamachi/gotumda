(clack.util:namespace gotumda.controller
  (:use :cl
        :caveman
        :gotumda
        :anaphora)
  (:import-from :gotumda.view.emb
                :render)
  (:import-from :gotumda.model
                :<task>
                :task-body
                :get-task-by-id
                :get-all-tasks)
  (:import-from :elephant
                :drop-instance)
  (:import-from :gotumda.util.elephant
                :object-id)
  (:import-from :clack.response
                :headers))

(cl-annot:enable-annot-syntax)

@url ANY "/api/*.json"
(defun api-controller (params)
  @ignore params
  (sunless (headers *response* :content-type)
    (setf it
          "application/json"))
  (next-route))

@url GET "/"
(defun index (params)
  "Show index page."
  (render
   "index.tmpl"
   params))

@url POST "/api/update.json"
(defun update (params)
  "Create/Edit a task."
  (let ((task (aif (getf params :|id|)
                   (get-task-by-id it)
                   (make-instance '<task>))))
    (setf (task-body task) (getf params :|body|))
    (princ-to-string task)))

@url POST "/api/destroy.json"
(defun destroy (params)
  "Delete a task."
  (aif (get-task-by-id (getf params :|id|))
       (progn (drop-instance it)
              "true")
       "false"))

@url GET "/api/all-tasks.json"
(defun all-tasks (params)
  "Get task list through API. Return body is JSON."
  @ignore params
  (format nil "[~{~A~^,~}]"
          (get-all-tasks)))
