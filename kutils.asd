;;;; kutils.asd

(asdf:defsystem #:kutils
  :description "Kyle's utility package"
  :author "K. Isom <kyle@metacircular.net>"
  :license "MIT License"
  :serial t
  :components ((:file "package")
	       (:file "on")
	       (:file "lol")
               (:file "kutils")
               (:file "kutils-hash-tables")
	       (:file "macros")))
