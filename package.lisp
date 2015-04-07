;;;; package.lisp

(defpackage #:kutils
  (:use #:cl)
  (:export #:mkstr     ; On Lisp utilities
	   #:symbb
	   #:group
	   #:flatten
	   #:compose
	   #:defmacro! ; Let Over Lambda utilities
	   #:join      ; My utilities
	   #:build-list
	   #:partial
           #:enable-hash-table-reader
           #:hashkeys
	   #:sethash
           #:hash-table-to-alist
	   #:alist-to-hash-table
	   ))
