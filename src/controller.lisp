(clack.util:namespace gotanda.controller
  (:use :cl
        :caveman
        :gotanda)
  (:import-from :gotanda.view.emb
                :render))

(cl-annot:enable-annot-syntax)

@url ANY "/api/*"
(defun api-controller (params)
  @ignore params
  (next-route)
  )

@url ANY "/pc/*"
(defun pc-controller (params)
  @ignore params
  (next-route)
  )

@url GET "/?:dev?/"
(defun index (params)
  (render
   "index.tmpl"
   params))

@url POST "/?:dev?/update"
(defun update (params)
  @ignore params
  )
