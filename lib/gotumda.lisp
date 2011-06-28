(clack.util:namespace gotumda
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.clsql)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>
                :app-mode)
  (:import-from :elephant
                :open-store
                :close-store)
  (:import-from :trivial-shell
                :shell-command))

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
(defun reload ()
  (let* ((config (caveman.app:config *app*))
         (js-dir (reduce
                  #'merge-pathnames
                  (list #p"static/js/"
                        (getf config :static-path)
                        (getf config :application-root))))
         (compiler-path (merge-pathnames
                         #p"tool/compiler.jar"
                         (getf config :application-root))))
    (trivial-shell:shell-command
     (format nil
      "python ~A~:*closure-library/closure/bin/build/depswriter.py --root_with_prefix=\"~A~:*got ../../../got\" --output_file=~Adeps.js"
      js-dir))
    (when (eq (app-mode *app*) :prod)
      (trivial-shell:shell-command
       (format nil
        "python ~A~:*closure-library/closure/bin/build/closurebuilder.py --root=~A~:*closure-library --root=~A~:*got -n got.app.PC -o compiled --output_file=~Acompiled.js -c ~A -f \"--define=goog.DEBUG=false\""
        js-dir
        compiler-path)))))

@export
(defun start (&key (mode :dev) (debug t) lazy)
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy)
  (let* ((config (caveman.app:config *app*))
         (dbtype (getf config :database-type))
         (dbspec (getf config :database-connection-spec)))
    (reload)
    (open-store `(:clsql (,dbtype ,dbspec)))))

@export
(defun stop ()
  (close-store)
  (caveman.app:stop *app*))
