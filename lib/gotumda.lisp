(clack.util:namespace gotumda
  (:use :cl
        :clack
        :clack.builder
        :clack.middleware.clsql
        :gotumda.middleware.authhatena)
  (:shadow :stop)
  (:import-from :caveman.app
                :<app>
                :build
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

(defmethod build ((this gotumda) &optional (app this))
  (let ((config (caveman.app:config this)))
    (call-next-method
     this
     (builder
      (<gotumda-middleware-authhatena>
       :consumer-key (getf config :consumer-key)
       :consumer-secret (getf config :consumer-secret)
       :authorize-uri (getf config :authorize-uri)
       :cert-uri (getf config :cert-uri)
       :callback-uri "http://localhost:8080/"
       :auth-path "/auth")
      app))))

@export
(defun rebuild-js ()
  "Rebuild the dependency of JS files."
  (let* ((config (caveman.app:config *app*))
         (js-dir (reduce
                  #'merge-pathnames
                  (list #p"static/js/"
                        (getf config :static-path)
                        (getf config :application-root)))))
    (trivial-shell:shell-command
     (format nil
      "python ~A~:*closure-library/closure/bin/build/depswriter.py \\~
       ~%--root_with_prefix=\"~A~:*got ../../../got\" \\~
       ~%--output_file=~Adeps.js"
      js-dir))))

@export
(defun compile-js ()
  "Compile JS files into one file by Closure Compiler."
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
      "python ~A~:*closure-library/closure/bin/build/closurebuilder.py \\~
       ~%--root=~A~:*closure-library --root=~A~:*got -n got.app.PC \\~
       ~%-o compiled --output_file=~Acompiled.js -c ~A \\~
       ~%-f \"--define=goog.DEBUG=false\""
      js-dir
      compiler-path))))

@export
(defun start (&key (mode :dev) (debug t) lazy)
  (caveman.app:start *app* :mode mode :debug debug :lazy lazy)
  (let* ((config (caveman.app:config *app*))
         (dbtype (getf config :database-type))
         (dbspec (getf config :database-connection-spec)))
    (rebuild-js)
    (when (eq mode :prod)
      (compile-js))
    (open-store `(:clsql (,dbtype ,dbspec)))))

@export
(defun stop ()
  (close-store)
  (caveman.app:stop *app*))
