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
	   #:sethash
	   ))

