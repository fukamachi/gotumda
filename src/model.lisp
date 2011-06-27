(clack.util:namespace gotumda.model
  (:use :cl
        :caveman)
  (:import-from :elephant
                :persistent-metaclass
                :*store-controller*
                :controller-recreate-instance
                :drop-instance
                :get-instances-by-class)
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
  (remove-if #'deleted-p
             (get-instances-by-class '<task>)))
