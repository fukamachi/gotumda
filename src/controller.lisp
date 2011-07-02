(clack.util:namespace gotumda.controller
  (:use :cl
        :caveman
        :gotumda
        :anaphora
        :split-sequence)
  (:import-from :gotumda.view.emb
                :render)
  (:import-from :gotumda.model.task
                :<task>
                :task-body
                :task-user
                :task-owner
                :is-done
                :get-task-by-id
                :get-all-tasks
                :task-order
                :copy-task)
  (:import-from :elephant
                :with-transaction
                :drop-instance)
  (:import-from :gotumda.util.elephant
                :object-id)
  (:import-from :clack.response
                :headers)
  (:import-from :gotumda.model.user
                :current-user
                :user-plist))

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
   "index.html"
   `(:user ,(aand (current-user) (user-plist it))
     ,@params)))

@url POST "/api/update.json"
(defun update (params)
  "Create/Edit a task. Allowed parameters are `id', `body' and `is-done'."
  (with-transaction ()
   (let ((task (aif (getf params :|id|)
                    (get-task-by-id it)
                    (make-instance '<task>))))
     (cond
       ((string= "false" (getf params :|isDone|))
        (setf (is-done task) nil))
       ((string= "true" (getf params :|isDone|))
        (setf (is-done task) t)))
     (awhen (getf params :|body|)
       (setf (task-body task) it))
     (setf (task-user task) (current-user))
     (setf (task-owner task) (current-user))
     (princ-to-string task))))

@url POST "/api/copy.json"
(defun copy (params)
  "Copy a task to my task."
  (awhen (getf params :|id|)
    (with-transaction ()
      (let ((task (copy-task (get-task-by-id it))))
        (setf (task-owner task) (current-user))
        (setf (task-user task) (current-user))
        (princ-to-string task)))))

@url POST "/api/move.json"
(defun move (params)
  "Move the task to my task."
  (awhen (getf params :|id|)
    (with-transaction ()
      (let ((task (get-task-by-id it)))
        (setf (task-owner task) (current-user))
        (princ-to-string task)))))

@url POST "/api/destroy.json"
(defun destroy (params)
  "Delete a task."
   (aif (get-task-by-id (getf params :|id|))
        (with-transaction ()
         (drop-instance it)
         "true")
        "false"))

@url GET "/api/all-tasks.json"
(defun all-tasks (params)
  @ignore params
  "Get task list through API. Return value is a JSON string."
  (format nil "[~{~A~^,~}]"
          (get-all-tasks)))

@url POST "/api/sort-tasks.json"
(defun sort-tasks (params)
  "Sort tasks through API."
  (setf (task-order)
        (mapcar #'parse-integer
                (split-sequence #\, (getf params :|order|))))
  (format nil "[~{~A~^,~}]" (coerce (task-order) 'list)))
