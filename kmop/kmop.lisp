;;;; kmop.lisp

(in-package #:kutils-mop)

;;; metaobject interactions

(defun sym-lookup (sym &optional (package *package*))
  (find-symbol (mkstr sym) package))

(defun class-symbol (sym &optional (package *package*))
  (cond
    ((symbolp sym)
     (whenlet (sym (sym-lookup sym package))
       (find-class sym)))
    ((keywordp sym) nil)
    (t sym)))

(defvar *standard-object* (class-symbol 'standard-object))

(defun subclassp (child parent)
  (whenlet ((child  (class-symbol child))
	    (parent (class-symbol parent)))
    (closer-mop:subclassp child parent)))

(defun objectp (sym)
  (subclassp sym *standard-object*))

(defun get-class-initargs (slots)
  (flatten
   (mapcar #'closer-mop:slot-definition-initargs slots)))


(defun get-all-slots (class-sym &optional (package *package*))
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

(defun kw-key (slot &optional snake-case)
  (let ((s (string-downcase
	    (mkstr
	     (first
	      (closer-mop:slot-definition-initargs slot))))))
    (if snake-case
	(nsubstitute #\- #\_ s)
	(nsubstitute #\_ #\- s))))

(defun dispatch-get-value (slot value)
  (let ((slot-type (closer-mop:slot-definition-type slot)))
    (if (objectp slot-type)
	(make-instance-from-hash-table slot-type value)
	value)))

(defun slot-initarg (slot)
  (first
   (closer-mop:slot-definition-initargs slot)))

(defun slot-table-value (slot ht &optional snake-case)
  (when ht
    (gethash (kw-key slot snake-case) ht)))

(defun zip-initargs-hash-table (slots ht &optional snake-case)
  (flatten
   (remove-if #'null
	      (mapcar (lambda (slot)
			(whenlet ((kwarg (slot-initarg slot))
				  (value (dispatch-get-value
					  slot
					  (slot-table-value
					   slot ht snake-case))))
			  (list kwarg value)))
		      slots))))

(defun make-instance-from-hash-table
    (class-type table
     &optional (package *package*) snake-case)
  "Given a class symbol and a hash table, attempt to build an instance
from it. The instance initargs are derived from the slot definitions,
and an attempt is made to pair the slot with a string derivation. It
is expected that the hash table keys will be downcase. If
@c(snake-case) is t, the keys should use hyphens; otherwise, they
should use underscores. The slot type is used to determine whether
to attempt to parse another object as a hash table entry."
  (whenlet ((class-sym (class-symbol class-type))
	    (arglst (zip-initargs-hash-table
		     (get-all-slots class-type package)
		     table snake-case)))
    (apply #'make-instance class-sym arglst)))
