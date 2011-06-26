(clack.util:namespace gotumda.model
  (:use :cl
        :caveman)
  (:import-from :elephant
                :persistent-metaclass
                :*store-controller*
                :controller-recreate-instance)
  (:import-from :clack.response
                :headers)
  (:export :task-body))

(cl-annot:enable-annot-syntax)

@export
(defclass <task> ()
     ((body :type string
            :initarg body
            :accessor task-body)
      (tags :type list
            :initform nil
            :accessor tags))
  (:metaclass persistent-metaclass))

(defmethod initialize-instance :after ((this <task>) &key)
  ;; TODO: read `body' and put `tags'.
  )

@export
(defmethod task-id ((this <task>))
  (slot-value this 'elephant::oid))

@export
(defmethod dropped-task-p ((this <task>))
  (not (and (task-id this)
            (slot-boundp this 'body))))

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
          (task-id this)
          (task-body this)))

@export
(defun find-task-by-id (oid)
  (let ((task
         (elephant::controller-recreate-instance
          *store-controller*
          oid '<task>)))
    (when (slot-boundp task 'body)
      task)))
