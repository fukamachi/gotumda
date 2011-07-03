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
               :arnesi
               :trivial-shell
               :split-sequence
               :ironclad
               :drakma
               :cl-json)
  :components ((:module "lib"
                :components
                ((:file "gotumda" :depends-on ("middleware"))
                 (:module "view"
                  :depends-on ("gotumda")
                  :components
                  ((:file "emb")))
                 (:module "middleware"
                  :components
                  ((:file "authhatena")))))
               (:module "src"
                :depends-on ("lib")
                :components
                ((:module "util"
                  :components
                  ((:file "elephant")))
                 (:file "controller" :depends-on ("util" "model"))
                 (:module "model"
                  :depends-on ("util")
                  :components
                  ((:file "task" :depends-on ("user"))
                   (:file "user")))))))
