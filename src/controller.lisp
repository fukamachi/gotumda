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
                :find-task-by-id)
  (:import-from :elephant
                :get-instances-by-class)
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

@url GET "/?:device?/"
(defun index (params)
  "Show index page."
  (render
   "index.tmpl"
   params))

@url POST "/?:device?/update"
(defun update (params)
  "Create/Edit a task."
  (let ((task (aif (getf params :id)
                   (find-task-by-id it)
                   (make-instance '<task>))))
    (setf (task-body task) (getf params :body))))

@url POST "/?:device?/destroy/:id"
(defun destroy (params)
  "Delete a task."
  (awhen (find-task-by-id (getf params :id))
    (drop-instance it)))

@url GET "/?:device?/all-tasks"
(defun all-tasks (params)
  "Get task list."
  @ignore params
  (princ-to-string (get-instances-by-class '<task>)))

@url GET "/?:device?/task/:id"
(defun task (params)
  (awhen (find-task-by-id (getf params :id))
    (princ-to-string it)))
