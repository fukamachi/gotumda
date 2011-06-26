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
      (tags :type list
            :initform nil
            :accessor tags)
      (deleted-p :type boolean
                 :initform nil
                 :accessor deleted-p)
      (done-p :type boolean
              :initform nil
              :accessor done-p))
  (:metaclass persistent-metaclass))

(defmethod initialize-instance :after ((this <task>) &key)
  ;; TODO: read `body' and put `tags'.
  )

(defmethod print-object ((this <task>) stream)
  (let ((content-type (and *response*
                           (headers *response* :content-type))))
    (cond
      ((string= content-type "text/html")
       (print-object-html this stream))
      ((string= content-type "application/json")
       (print-object-json this stream))
      (t (call-next-method)))))

(defmethod print-object-html ((this <task>) stream)
  "HTML representation of `<task>'."
  (format stream
          "~A"
          (task-body this)))

(defmethod print-object-json ((this <task>) stream)
  "JSON representation of `<task>'."
  (format stream
          "{\"id\":\"~A\",\"body\":\"~A\"}"
          (object-id this)
          (task-body this)))

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
