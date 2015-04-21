;;;; kutils.lisp

(in-package #:kutils)

;;; "kutils" goes here. Hacks and glory await!

;;; This file contains various utilities I've written.

(defun take (n lst)
  "Take n elements from lst."
  (labels ((rec (n lst acc)
             (if (or (zerop n) (null lst))
                 (nreverse acc)
                 (rec (- n 1) (cdr lst) (cons (car lst) acc)))))
    (when (> n 0)
      (rec n lst nil))))

(defun drop (n lst)
  "Drop n elements from the list."
  (if (or (<= n 0) (null lst))
      lst
      (drop (- n 1) (cdr lst))))

(defun interpose (x sep)
  "Takes a list and a separator, and places separator between element
of the list."
  (let ((x (coerce x 'list)))
    (apply #'append
	   (mapcar (lambda (y)
		     (list y sep))
		   x))))

(defun build-list (arg)
  "If arg is an atom, return it as a list. If it's a list, return the
arg. If it's a vector, coerce it to a list. Otherwise, return nil."
  (cond
    ((listp   arg) (copy-list arg))
    ((vectorp arg) (coerce arg 'list))
    ((atom    arg) (list arg))
    (t nil)))

(defun partial (fn &rest initial-args)
  "partial provides partial function application. It returns a lambda
that will call the function given with the intial args and any
additional args provided to the lambda."
  (lambda (&rest args)
    (apply fn (append initial-args args))))

(defun macroexpand-n (n form)
  "Expand the macro n times."
  (let ((new-form form))
    (dotimes (i n)
      (multiple-value-bind
	    (expansion expanded)
	  (macroexpand-1 new-form)
	(if expanded
	    (setf new-form expansion)
	    (return))))
    new-form))

(defun mksymb (&rest args)
  "Create a symbol from arguments, upcasing strings as required."
  (let ((args (mapcar (lambda (arg)
			(if (stringp arg)
			    (string-upcase arg)
			    arg))
		      args)))
    (apply #'symb args)))

(defun mkkw (&rest args)
  "Create a keyword from its arguments."
  (intern (string (apply #'mksymb args)) "KEYWORD"))

(defun build-slot (class-name slot)
  (let* ((slot-sym (mksymb slot))
	 (accessor (mksymb class-name "-" slot-sym)))
    (list slot-sym
	  :initarg (mkkw slot-sym)
	  :accessor accessor)))

(defun build-slot-list (class-name &rest slots)
  (mapcar (partial #'build-slot class-name) slots))

(defun build-arg-list (slots)
  (mapcar (lambda (slot) (list (mkkw slot) (mksymb slot))) slots))

(defun inherited-slots (supers)
  (mapcar #'closer-mop:slot-definition-name
	  (flatten
	   (mapcar #'closer-mop:class-slots
		   (mapcar (lambda (cls) (find-class cls t nil)) supers)))))

(defun superclasses (superclasses)
  (mapcar #'class-name
	  (mapcar (lambda (cls) (find-class cls t nil))
		  superclasses)))

(defmacro defclass! (name superclass-spec slots &body body)
  "Defines a new class and default constructor for name, based on the
superclasses and slots provided. If the first argument to body is a
string, it will be used as the class's docstring."
  (let* ((name (mksymb name))
	 (docstring (if (stringp (first body))
		      (list :documentation (first body))
		      (list :documentation (format nil"Automatically generated class."))))
	 (supers (superclasses superclass-spec))
	 (body (if docstring (rest body) body))
	 (ctor (mksymb "make-" name))
	 (all-slots (flatten (append (inherited-slots supers) slots))))
    (format t "supers: ~A~%" supers)
    `(progn
       (closer-mop:ensure-finalized
	(defclass ,name ,supers
	  ,(loop for slot in slots collecting
		(list slot :initarg (mksymb slot)
                      :accessor (mksymb name #\- slot)))
	  ,docstring
	  ,@body))
       t)))

(defun zip (&rest lsts)
  "Zip together elements from each list: (zip '(a b c) '(1 2 3))
produces '((a 1) (b 2) (c 3))."
  (labels ((zip-acc (lsts)
	     (unless (some #'null lsts)
	       (cons (mapcar #'car lsts) (zip-acc (mapcar #'cdr lsts))))))
    (zip-acc lsts)))


;;; hash-table functions.


(defun |#{-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((m     (make-hash-table :test 'equal))
	(k     nil)
	(v     nil)
	(key-p t))
    (labels ((finalise-read (x)
	       (reverse (concatenate 'string x)))
	     (finalise-kv-pair ()
	       (if key-p
		   (unless (null k)
		     (setq key-p nil
			   k     (read-from-string (finalise-read k))))
		   (unless (null v)
		     (setq key-p t
			   v     (read-from-string (finalise-read v)))
		     (setf (gethash k m) v)
		     (setq k nil v nil))))
	     (reading-complete-p ()
	       (and (null v)
		    (not
		     (null k)))))
      (do ((prev (read-char stream) curr)
	   (curr (read-char stream) (read-char stream)))
	  ((and (char= prev #\}) (char= curr #\#)))
	(if (char= prev #\Space)
	    (finalise-kv-pair)
	    (if key-p
		(push prev k)
		(push prev v))))
      (if (reading-complete-p)
	  (error "Mismatched key value pairs.")
	  (progn 
	    (finalise-kv-pair)
	    m)))))

(defun enable-hash-table-reader ()
  "Enables the reader macro #{}# for hash-tables. The resulting
hash-table will use #'equal for equality. For example,

#{:a :b :c :d}#

will create a hash-table with the keys :a and :c. :a stores the
value :b, and :c stores the value :d.
"
  (set-dispatch-macro-character
   #\# #\{ #'|#{-reader|))

(defun sethash (k v m)
  "Convenience notation for setting a value in a hash table."
  (setf (gethash k m) v))

(defun hashkeys (m)
  "Returns a list of the keys in the hash table."
  (let ((keys '()))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     m)
    keys))

(defun hash-table-to-alist (m)
  "Converts the hash-table given to an alist of (key . value) pairs."
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (setf alist (cons elt alist))))
	     m)
    alist))

(defun alist-to-hash-table (alist)
  "Converts the alist to a hash-table."
  (let ((m (make-hash-table :test 'equal)))
    (dolist (elt alist)
      (sethash (car elt) (cdr elt) m))
    m))


(defun copy-hash-table (ht)
  (let ((copied (make-hash-table :equal #'equal)))
    (maphash (lambda (k v)
	       (sethash k v copied))
	     ht)
    copied))

       ;; (defun ,ctor (&key ,@(append (inherited-slots supers)
       ;; 				    slots))
       ;; 	 (make-instance (find-class ,name t nil)
       ;; 			,@(flatten (build-arg-list all-slots))))
