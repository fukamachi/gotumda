(clack.util:namespace gotumda.model.task
  (:use :cl
        :caveman
        :anaphora)
  (:import-from :elephant
                :persistent-metaclass
                :*store-controller*
                :controller-recreate-instance
                :drop-instance
                :get-instances-by-class
                :add-to-root
                :get-from-root
                :root-existsp
                :with-transaction)
  (:import-from :json
                :encode-json)
  (:import-from :clack.response
                :headers)
  (:import-from :gotumda.util.elephant
                :object-id
                :get-instance-by-id)
  (:export :task-body
           :is-deleted
           :is-done))

(cl-annot:enable-annot-syntax)

(defun make-task-order (&optional initial-contents)
  "Create a new array for task list."
  (make-array (length initial-contents)
              :element-type 'integer
              :fill-pointer t
              :adjustable t
              :initial-contents initial-contents))

@export
(defun task-order ()
  "Return a persistent collection of Elephant which represents Tasks order.
If it doesn't exist, creates new one and add it."
  (if (root-existsp "task-order")
      (get-from-root "task-order")
      (with-transaction ()
        (add-to-root "task-order"
                     (make-task-order)))))

@export
(defun (setf task-order) (order)
  (with-transaction ()
    (add-to-root "task-order"
                 (make-task-order order))))

@export
(defclass <task> ()
     ((body :type string
            :initarg :body
            :accessor task-body)
      (user :type (or string <user>)
            :initarg :user
            :accessor task-user)
      (owner :type (or string <user>)
             :initarg :owner
             :accessor task-owner)
      (origin-id :type (or string integer null)
                 :initarg :origin-id
                 :initform nil
                 :accessor task-origin-id)
      (is-deleted :type boolean
                  :initform nil
                  :accessor is-deleted)
      (is-done :type boolean
               :initform nil
               :accessor is-done))
  (:metaclass persistent-metaclass))

(defmethod initialize-instance :after ((this <task>) &key)
  "Put the object ID into the task order collection."
  (vector-push-extend (object-id this) (task-order))
  (with-transaction ()
    (add-to-root "task-order"
                 (task-order))))

(defmethod print-object ((this <task>) stream)
  (let ((content-type (and *response*
                           (headers *response* :content-type))))
    (if (string= content-type "application/json")
        (json:encode-json
         `((:|task-id| . ,(object-id this))
           (:|body| . ,(task-body this))
           (:|user| . ,(task-user this))
           (:|owner| . ,(task-owner this))
           (:|origin-task| . ,(awhen (task-origin-id this)
                                (get-task-by-id it)))
           (:|is-deleted| . ,(is-deleted this))
           (:|is-done| . ,(is-done this)))
          stream)
        (call-next-method))))

@export
(defmethod copy-task ((this <task>))
  (make-instance '<task>
     :body (task-body this)
     :user (task-user this)
     :owner (task-owner this)
     :origin-id (object-id this)
     :is-deleted (is-deleted this)
     :is-done (is-done this)))

@export
(defun get-task-by-id (id)
  "Find a task and return it by the object id."
  (let ((task (get-instance-by-id '<task> id)))
    (unless (is-deleted task)
      task)))

(defmethod drop-instance :after ((this <task>))
  "For `elephant:drop-instance'. Set `is-deleted' T when it is dropped."
  (setf (is-deleted this) t))

@export
(defun get-all-tasks ()
  "This is similar to `elephant:get-instances-by-class',
but this returns sorted task list."
  (sort
   (remove-if #'is-deleted
              (get-instances-by-class '<task>))
   (lambda (a b)
     (loop with order = (task-order)
           with a = (object-id a)
           with b = (object-id b)
           for id across order
           do (cond
                ((= a id) (return t))
                ((= b id) (return nil)))))))