;;;; kmop.lisp

(in-package #:kutils-mop)

;;; metaobject interactions

(defun get-class-initargs (slots)
)


(defun get-all-init-args (class-sym &optional (package *package*))
  (let ((class-val (find-class
			  (find-symbol
			   (mkstr class-sym)
			   package))))
    (when class-val
      (closer-mop:ensure-finalized class-val)
      (flatten
       (remove-if (lambda (slot)
		    (null (closer-mop:slot-definition-initargs slot)))
		  (closer-mop:class-slots class-val))))))

(defun kw-key (kw)
  (string-downcase
   (mkstr kw)))

(defun dispatch-get-value (value))


(defun zip-initargs-hash-table (args ht)
  (flatten
   (mapcar (lambda (slot)
	     (list slot (gethash (kw-key slot) ht)))
	   args)))

(defun make-instance-from-hash-table (class-type table &optional (package *package*))
  (let ((class-sym (find-class class-type)))
    (when class-sym
      (apply #'make-instance class-sym
	     (zip-initargs-hash-table
	      (get-all-init-args class-type package)
	      table)))))
