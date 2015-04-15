;;;; package.lisp

(defpackage #:kutils
  (:use #:cl)
  (:export #:mkstr     ; On Lisp utilities
           #:symb
           #:group
           #:flatten
           #:compose
           #:defmacro! ; Let Over Lambda utilities
           #:interpose ; My utilities
           #:build-list
           #:partial
           #:macroexpand-n
           #:mksymb
           #:mkkw
           #:defclass!
	   #:zip
           #:enable-hash-table-reader
           #:hashkeys
           #:sethash
           #:hash-table-to-alist
           #:alist-to-hash-table
           ))
