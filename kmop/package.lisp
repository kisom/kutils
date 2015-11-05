;;;; package.lisp

(defpackage #:kutils-mop
  (:use #:cl #:kutils)
  (:export #:make-instance-from-hash-table
	   #:list-all-slots))
