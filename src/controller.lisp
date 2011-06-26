(clack.util:namespace gotumda.controller
  (:use :cl
        :caveman
        :gotumda
        :anaphora)
  (:import-from :gotumda.view.emb
                :render)
  (:import-from :gotumda.model
                :<task>
                :task-id
                :task-body
                :find-task-by-id)
  (:import-from :elephant
                :get-instances-by-class
                :drop-instance)
  (:import-from :clack.response
                :headers))

(cl-annot:enable-annot-syntax)

@url ANY "/api/*"
(defun api-controller (params)
  @ignore params
  (sunless (headers *response* :content-type)
    (setf it
          "application/json"))
  (next-route))

@url ANY "/?p?c?/*"
(defun pc-controller (params)
  @ignore params
  (sunless (headers *response* :content-type)
    (setf it
          "text/html"))
  (next-route))

@url GET "/"
(defun index (params)
  "Show index page."
  (render
   "index.tmpl"
   params))

@url POST "/api/update"
(defun update (params)
  "Create/Edit a task."
  (let ((task (aif (getf params :|id|)
                   (find-task-by-id it)
                   (make-instance '<task>))))
    (setf (task-body task) (getf params :|body|))
    (princ-to-string task)))

@url POST "/api/destroy/:id"
(defun destroy (params)
  "Delete a task."
  (awhen (find-task-by-id (getf params :id))
    (drop-instance it)))

@url GET "/api/all-tasks"
(defun all-tasks (params)
  "Get task list through API. Return body is JSON."
  @ignore params
  (format nil "[~{~A~^,~}]"
          (get-instances-by-class '<task>)))
