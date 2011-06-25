#|
  This file is a part of Gotumda project.
|#

(in-package :cl-user)
(defpackage gotumda-test-asd
  (:use :cl :asdf))
(in-package :gotumda-test-asd)

(defsystem gotumda-test
  :author "Eitarow"
  :license "Apache"
  :depends-on (:gotumda
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "gotumda")))))
