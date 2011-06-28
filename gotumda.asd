#|
  This file is a part of Gotumda project.
|#

(in-package :cl-user)
(defpackage gotumda-asd
  (:use :cl :asdf))
(in-package :gotumda-asd)

(defsystem gotumda
  :author "Eitarow Fukamachi"
  :license "Apache License 2.0"
  :version "0.1-SNAPSHOT"
  :depends-on (:caveman
               :cl-annot
               :elephant
               :anaphora
               :trivial-shell
               :split-sequence)
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
                ((:module "util"
                  :components
                  ((:file "elephant")))
                 (:file "controller" :depends-on ("util" "model"))
                 (:file "model" :depends-on ("util"))))))
