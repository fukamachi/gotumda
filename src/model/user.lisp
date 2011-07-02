(clack.util:namespace gotumda.model.user
  (:use :cl
        :caveman
        :arnesi)
  (:import-from :elephant
                :persistent-metaclass
                :get-instance-by-value))

(cl-annot:enable-annot-syntax)

@export
(defclass <user> ()
     ((name :type string
            :initarg :name
            :accessor user-name
            :index t)
      (image-url :type string
                 :initarg :image-url
                 :accessor image-url)
      (thumbnail-url :type string
                     :initarg :thumbnail-url
                     :accessor thumbnail-url)
      (tasks :type list
             :initform nil
             :accessor user-tasks))
  (:metaclass persistent-metaclass))

@export
(defun find-user (name)
  (get-instance-by-value '<user> 'name name))

@export
(defun current-user ()
  (aand (gethash :hatena.user *session*)
        (or (find-user (getf it :name))
            (make-instance '<user>
               :name (getf it :name)
               :image-url (getf it :image--url)
               :thumbnail-url (getf it :thumbnail--url)))
        it))

@export
(defmethod user-plist ((this <user>))
  (list
   :name (user-name this)
   :image-url (image-url this)
   :thumbnail-url (thumbnail-url this)
   :tasks (user-tasks this)))
