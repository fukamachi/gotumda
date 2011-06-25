(clack.util:namespace gotumda.controller
  (:use :cl
        :caveman
        :gotumda
        :anaphora)
  (:import-from :gotumda.view.emb
                :render)
  (:import-from :gotumda.model
                :<task>)
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
  (make-instance '<task>))

@url POST "/?:device?/destroy/:id"
(defun destroy (params)
  "Delete a task."
  (awhen (gotumda.model:find-task-by-id (getf params :id))
    (drop-instance it)))

@url GET "/?:device?/tasks"
(defun tasks (params)
  "Get task list."
  @ignore params
  (princ-to-string (get-instances-by-class '<task>)))
