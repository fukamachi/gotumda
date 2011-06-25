(clack.util:namespace gotumda.model
  (:use :cl
        :caveman)
  (:import-from :elephant
                :persistent-metaclass)
  (:import-from :clack.response
                :headers))

(cl-annot:enable-annot-syntax)

@export
(defclass <task> ()
     ((body :type string
            :initarg body
            :accessor body)
      (tags :type list
            :initform nil
            :accessor tags))
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
          (body this)))

(defmethod print-object-json ((this <task>) stream)
  "JSON representation of `<task>'."
  (format stream
          "{\"id\":\"~A\",\"body\":\"~A\"}"
          (slot-value this 'elephant::oid)
          (body this)))
