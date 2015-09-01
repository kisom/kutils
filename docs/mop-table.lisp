(ql:quickload :kutils-mop)
(ql:quickload :yason)

(defpackage #:kutils-example
  (:use #:cl #:kutils #:kutils-mop))

(in-package :kutils-example)

 (defclass class-a ()
    ((name :initarg :name
	   :accessor name-of)
     (value :initarg :value
	    :accessor value-of)))

(defun new-class-a (n v)
  (make-instance 'class-a :name n :value v))

(defclass reference ()
  ((link :initarg :link
	 :accessor link-of)
   (title :initarg :title
	  :accessor title-of)))

(defun new-reference (l title)
  (make-instance 'reference :link l :title title))

(defclass class-b (class-a)
  ((tag :initarg :tag
	:accessor tag-of)))

(defun new-class-b (n v tag)
  (make-instance 'class-b :name n :value v :tag tag))

(defclass class-c (class-a)
  ((initialised :initform nil)))

(defun new-class-c (n v)
  (make-instance 'class-c :name n :value v))

(defclass class-d (class-a)
  ((reference :initarg ref
	       :type reference
	       :accessor reference-of)))

(defun new-class-d (n v ref)
  (make-instance 'class-a :name n :value v :ref ref))

(defvar *class-d-table*
  (yason:parse
   ;; there is an example.json in the docs directory of kutils.
   (read-file-as-string #P"/tmp/example.json")))

(let ((obj
       (make-instance-from-hash-table 'class-d *class-d-table*)))
  (describe obj)
  (describe (reference-of obj)))

