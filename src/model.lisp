(clack.util:namespace gotumda.model
  (:use :cl
        :anaphora
        :caveman)
  (:import-from :elephant
                :persistent-metaclass
                :*store-controller*
                :controller-recreate-instance
                :drop-instance
                :get-instances-by-class
                :make-pset
                :find-item
                :insert-item
                :add-to-root
                :get-from-root
                :with-transaction)
  (:import-from :clack.response
                :headers)
  (:import-from :gotumda.util.elephant
                :object-id
                :get-instance-by-id)
  (:export :task-body
           :deleted-p
           :done-p))

(cl-annot:enable-annot-syntax)

@export
(defun task-order ()
  "Return a persistent collection of Elephant which represents Tasks order.
If it doesn't exist, creates new one and add it."
  (aif (get-from-root "task-order")
       it
       (with-transaction ()
         (add-to-root "task-order" (make-pset)))))

@export
(defun (setf task-order) (order)
  (with-transaction ()
    (add-to-root "task-order" (make-pset :items order))))

@export
(defclass <task> ()
     ((body :type string
            :initarg body
            :accessor task-body)
      (url :type (or null string)
           :initarg url
           :initform nil
           :accessor task-url)
      (deleted-p :type boolean
                 :initform nil
                 :accessor deleted-p)
      (done-p :type boolean
              :initform nil
              :accessor done-p))
  (:metaclass persistent-metaclass))

(defmethod initialize-instance :after ((this <task>) &key)
  "Put the object ID into the task order collection."
  (with-transaction ()
    (insert-item (object-id this)
     (task-order))))

(defmethod print-object ((this <task>) stream)
  (let ((content-type (and *response*
                           (headers *response* :content-type))))
    (if (string= content-type "application/json")
        (format stream
                "{\"id\":\"~A\",\"body\":\"~A\",\"isDone\":\"~A\"}"
                (object-id this)
                (task-body this)
                (if (done-p this)
                    "true"
                    "false"))
        (call-next-method))))

@export
(defun get-task-by-id (id)
  (let ((task (get-instance-by-id '<task> id)))
    (unless (deleted-p task)
      task)))

(defmethod drop-instance :after ((this <task>))
  (setf (deleted-p this) t))

@export
(defun get-all-tasks ()
  (sort
   (remove-if #'deleted-p
              (get-instances-by-class '<task>))
   (lambda (a b)
     (< (find-item (object-id a) (task-order))
        (find-item (object-id b) (task-order))))))
