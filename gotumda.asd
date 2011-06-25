#|
  This file is a part of Gotumda project.
|#

(in-package :cl-user)
(defpackage gotumda-asd
  (:use :cl :asdf))
(in-package :gotumda-asd)

(defsystem gotumda
  :author "Eitarow Fukamachi"
  :license "Apache"
  :version "0.1-SNAPSHOT"
  :depends-on (:caveman
               :cl-annot
               :elephant
               :anaphora)
  :components ((:module "lib"
                :components
                ((:file "gotumda")
                 (:module "view"
                  :depends-on ("gotumda")
                  :components
                  ((:file "emb")))))
               (:module "src"
                :depends-on ("lib")
                :components
                ((:file "controller" :depends-on ("model"))
                 (:file "model")))))
