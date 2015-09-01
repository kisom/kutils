(defpackage #:kutils-example
  (:use #:cl #:kutils #:kutils-mop))

(in-package :kutils-example)

KUTILS-EXAMPLE> (defclass class-a ()
		  ((name :initarg :name
			 :accessor name-of)
		   (value :initarg :value
			  :accessor value-of)))
#<STANDARD-CLASS KUTILS-EXAMPLE::CLASS-A>
KUTILS-EXAMPLE> (defun new-class-a (n v)
		  (make-instance 'class-a :name n :value v))
NEW-CLASS-A
KUTILS-EXAMPLE> (defclass reference ()
		  ((link :initarg :link
			 :accessor link-of)
		   (title :initarg :title
			  :accessor title-of)))
#<STANDARD-CLASS KUTILS-EXAMPLE::REFERENCE>
KUTILS-EXAMPLE> (defun new-reference (l title)
		  (make-instance 'reference :link l :title title))
NEW-REFERENCE
KUTILS-EXAMPLE> (defclass class-b (class-a)
		  ((tag :initarg :tag
			:accessor tag-of)))
#<STANDARD-CLASS KUTILS-EXAMPLE::CLASS-B>
KUTILS-EXAMPLE> (defun new-class-b (n v tag)
		  (make-instance 'class-b :name n :value v :tag tag))
NEW-CLASS-B
KUTILS-EXAMPLE> (defclass class-c (class-a)
		  ((initialised :initform nil)))
#<STANDARD-CLASS KUTILS-EXAMPLE::CLASS-C>
KUTILS-EXAMPLE> (defun new-class-c (n v)
		  (make-instance 'class-c :name n :value v))
NEW-CLASS-C
KUTILS-EXAMPLE> (defclass class-d (class-a)
		  ((reference :initarg ref
			      :type reference
			      :accessor reference-of)))
#<STANDARD-CLASS KUTILS-EXAMPLE::CLASS-D>
KUTILS-EXAMPLE> (defun new-class-d (n v ref)
		  (make-instance 'class-a :name n :value v :ref ref))
NEW-CLASS-D
KUTILS-EXAMPLE> (defvar *class-d-table*
		  (yason:parse
		   ;; there is an example.json in the docs directory
		   ;; of kutils.
		   (read-file-as-string #P"/tmp/example.json")))
*CLASS-D-TABLE*
KUTILS-EXAMPLE> (let ((obj
		       (make-instance-from-hash-table
			'class-d *class-d-table*)))
		  (describe obj)
		  (describe (reference-of obj)))
#<CLASS-D {1007F0B013}>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME       = "something"
  VALUE      = "just a thing"
  REFERENCE  = #<REFERENCE {1007F0A853}>
#<REFERENCE {1007F0A853}>
  [standard-object]

Slots with :INSTANCE allocation:
  LINK   = "https://common-lisp.net/"
  TITLE  = "Common Lisp"
; No value
