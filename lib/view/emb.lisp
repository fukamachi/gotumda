(clack.util:namespace gotumda.view.emb
  (:use :cl)
  (:import-from :caveman
                :config))

(cl-annot:enable-annot-syntax)

@export
(defun render (file params)
  (caveman.view.emb:render
   (merge-pathnames file
    (merge-pathnames
     (config :template-path)
     (config :application-root)))
   params))
