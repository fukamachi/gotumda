(clack.util:namespace gotumda.model
  (:use :cl
        :caveman
        :gotumda)
  (:import-from :elephant
                :persistent-metaclass))

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

(defmethod initialize-method :after ((this <task>) &key)
  ;; TODO: read `body' and put `tags'.
  )
