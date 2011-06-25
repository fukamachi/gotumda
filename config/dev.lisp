`(:static-path #p"public/"
  :log-path #p"log/"
  :template-path #p"src/tmpl/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :gotanda))
  :server :hunchentoot
  :port 8080
  :database-type :sqlite3
  :database-connection-spec (,(namestring
                               (asdf:system-relative-pathname
                                :gotanda
                                "sqlite3.db"))))
