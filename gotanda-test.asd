#|
  This file is a part of gotanda project.
|#

(in-package :cl-user)
(defpackage gotanda-test-asd
  (:use :cl :asdf))
(in-package :gotanda-test-asd)

(defsystem gotanda-test
  :author ""
  :license ""
  :depends-on (:gotanda
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "gotanda")))))
