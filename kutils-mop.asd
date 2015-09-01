;;;; kutils-mop.asd

(asdf:defsystem #:kutils-mop
  :description "Kyle's MOP utilities."
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT License"
  :serial t
  :depends-on (#:closer-mop #:kutils)
  :components ((:file "kmop/package")
               (:file "kmop/kmop")))

