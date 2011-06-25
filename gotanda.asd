#|
  This file is a part of gotanda project.
|#

(in-package :cl-user)
(defpackage gotanda-asd
  (:use :cl :asdf))
(in-package :gotanda-asd)

(defsystem gotanda
  :version "0.1-SNAPSHOT"
  :depends-on (:caveman
               :cl-annot
               :elephant)
  :components ((:module "lib"
                :components
                ((:file "gotanda")
                 (:module "view"
                  :depends-on ("gotanda")
                  :components
                  ((:file "emb")))))
               (:module "src"
                :depends-on ("lib")
                :components
                ((:file "controller")))))
