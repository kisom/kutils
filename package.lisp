;;;; package.lisp

(defpackage #:kutils
  (:use #:cl)
  (:export ;; on.lisp : utilities from Graham's On Lisp.
           #:mkstr
           #:symb
           #:group
           #:flatten
           #:compose

	   ;; lol.lisp : utilities from Let Over Lambda
           #:defmacro!

	   ;; kutils.lisp
           #:interpose
           #:take
	   #:drop
           #:build-list
           #:partial
           #:macroexpand-n
           #:mksymb
           #:mkkw
           #:zip
           #:new-vector
           #:mapv
           #:build-vector
	   #:extend-vector
	   #:assoc-val
	   #:cartprod2
	   #:empty-or-nil-p
	   #:effector

	   ;; kutils-hash-tables.lisp
           #:enable-hash-table-reader
           #:hashkeys
           #:sethash
	   #:new-hash-table
	   #:with-new-hash-table
	   #:copy-hash-table
           #:hash-table-to-alist
           #:alist-to-hash-table
           ))
