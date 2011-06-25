(clack.util:namespace gotumda.controller
  (:use :cl
        :caveman
        :gotumda)
  (:import-from :gotumda.view.emb
                :render))

(cl-annot:enable-annot-syntax)

@url ANY "/api/*"
(defun api-controller (params)
  @ignore params
  (next-route)
  )

@url ANY "/pc/*"
(defun pc-controller (params)
  @ignore params
  (next-route)
  )

@url GET "/?:device?/"
(defun index (params)
  "Show index page."
  (render
   "index.tmpl"
   params))

@url POST "/?:device?/update"
(defun update (params)
  "Create/Edit a task."
  @ignore params
  (make-instance '<task>)
  )

@url POST "/?:device?/destroy/:id"
(defun destroy (params)
  "Delete a task."
  @ignore params
  )

@url GET "/?:device?/tasks"
(defun tasks (params)
  "Get task list."
  @ignore params
  (princ-to-string (get-instances-by-class '<task>))
  )
