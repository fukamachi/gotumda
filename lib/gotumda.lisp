(clack.util:namespace gotumda
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.clsql)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>)
  (:import-from :elephant
                :open-store
                :close-store))

(cl-annot:enable-annot-syntax)

@export
(defclass gotumda (<app>) ())

@export
(defvar *app* (make-instance 'gotumda))

(defmethod build ((this gotumda) app)
  (call-next-method
   this
   (builder
    (<clack-middleware-clsql>
     :database-type (getf (caveman.app:config this)
                          :database-type)
     :connection-spec (getf (caveman.app:config this)
                            :database-connection-spec)
     :connect-args '(:pool t :encoding :utf-8))
    app)))

@export
(defun start (&key (mode :dev) (debug t) lazy)
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy)
  (let* ((config (caveman.app:config *app*))
         (dbtype (getf config :database-type))
         (dbspec (getf config :database-connection-spec)))
    (open-store `(:clsql (,dbtype ,dbspec)))))

@export
(defun stop ()
  (close-store)
  (caveman.app:stop *app*))
