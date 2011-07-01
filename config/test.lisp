`(:static-path #p"public/"
  :log-path #p"log/"
  :template-path #p"src/tmpl/"
  :application-root ,(asdf:component-pathname
                      (asdf:find-system :gotumda))
  :consumer-key "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  :consumer-secret "xxxxxxxxxxxxxxxx"
  :authorize-uri "http://auth.hatena.ne.jp/auth"
  :cert-uri "http://auth.hatena.ne.jp/api/auth.json"
  :server :hunchentoot
  :port 4242
  :database-type :sqlite3
  :database-connection-spec ,(namestring
                              (asdf:system-relative-pathname
                               :gotumda
                               "t/test.db")))
