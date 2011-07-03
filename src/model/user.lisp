(clack.util:namespace gotumda.model.user
  (:use :cl
        :caveman
        :arnesi)
  (:import-from :elephant
                :persistent-metaclass
                :get-instance-by-value)
  (:import-from :clack.response
                :headers)
  (:import-from :json
                :encode-json)
  (:import-from :alexandria
                :plist-alist)
  (:export :user-projects))

(cl-annot:enable-annot-syntax)

@export
(defclass <user> ()
     ((name :type string
            :initarg :name
            :accessor user-name
            :index t)
      (image-url :type string
                 :initarg :image-url
                 :initform "/public/static/img/no-image.gif"
                 :accessor image-url)
      (thumbnail-url :type string
                     :initarg :thumbnail-url
                     :initform "/public/static/img/no-image-s.gif"
                     :accessor thumbnail-url)
      (projects :type list
                :initform nil
                :accessor user-projects))
  (:metaclass persistent-metaclass))

@export
(defun find-user (name &key force)
  (aif (get-instance-by-value '<user> 'name name)
       it
       (when force
         (make-instance '<user>
            :name name))))

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
   :thumbnail-url (thumbnail-url this)))

(defmethod print-object ((this <user>) stream)
  (let ((content-type (and *response*
                           (headers *response* :content-type))))
    (if (string= content-type "application/json")
        (json:encode-json
         (plist-alist (user-plist this))
         stream)
        (call-next-method))))
