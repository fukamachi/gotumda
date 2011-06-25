(clack.util:namespace gotanda
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.clsql)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>))

(cl-annot:enable-annot-syntax)

@export
(defclass gotanda (<app>) ())

@export
(defvar *app* (make-instance 'gotanda))

(defmethod build ((this gotanda) app)
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
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy))

@export
(defun stop ()
  (caveman.app:stop *app*))
