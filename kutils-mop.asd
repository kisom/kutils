;;;; kutils-mop.asd

(asdf:defsystem #:kutils-mop
  :description "Kyle's MOP utilities."
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT License"
  :serial t
  :pathname #P"kmop/"
  :depends-on (#:closer-mop #:kutils)
  :components ((:file "package")
               (:file "kmop")))

