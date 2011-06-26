(clack.util:namespace gotumda.util.elephant
  (:use :cl)
  (:import-from :elephant
                :persistent
                :*store-controller*
                :controller-recreate-instance))

(cl-annot:enable-annot-syntax)

@export
(defmethod object-id ((obj persistent))
  (handler-case
      (elephant::oid obj)
    (error ()
      (error "Object oid unbound for persistent instance: ~A" obj))))

@export
(defun get-instance-by-id (class id)
  (let ((obj
         (elephant::controller-recreate-instance
          *store-controller*
          id class)))
    obj))
