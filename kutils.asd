;;;; kutils.asd

(asdf:defsystem #:kutils
  :description "Kyle's utility package"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT License"
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
	       (:file "on")
	       (:file "lol")
               (:file "kutils")
               (:file "kutils-hash-tables")))

