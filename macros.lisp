;;;; macros.lisp

(in-package #:kutils)

;;; Various utility macros.

(defmacro whenlet (bindings &body body)
  "Evaluate the bindings in a let form; if they all evaluate to T,
evaluate @c(body) in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (when (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro whenlet* (bindings &body body)
  "Evaluate @c(bindings) in a @c(let*) form; if they all evaluate to T,
evaluate @c(body), which is wrapped in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (when (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro unlesslet (bindings &body body)
  "Evaluate the bindings in a let form; if they don't all evaluate to T,
evaluate @c(body) in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (unless (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro unlesslet* (bindings &body body)
  "Evaluate @c(bindings) in a @c(let*) form; if they are all not NIL,
evaluate @c(body), which is wrapped in an implicit @c(progn)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (unless (and ,@(mapcar #'first bindings))
	 ,@body))))

(defmacro iflet (bindings &body (then &optional else))
  "Evaluate @c(bindings) in a @c(let) form; if they are all T, execute
the @c(then) form. Otherwise, execute the @c(else) form."  
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (if (and ,@(mapcar #'first bindings))
	   (progn ,then)
	   (progn ,else)))))

(defmacro iflet* (bindings &body (then &optional else))
  "Evaluate @c(bindings) in a @c(let*) form; if they are all T, execute
the @c(then) form. Otherwise, execute the @c(else) form."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let* (,@bindings)
       (if (and ,@(mapcar #'first bindings))
	   (progn ,then)
	   (progn ,else)))))

(defmacro condlet (bindings &body forms)
  "Evaluate @c(bindings) in a @c(let) form, then evaluate @c(forms) in
a @c(cond)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (cond ,@forms))))

(defmacro condlet* (bindings &body forms)
  "Evaluate @c(bindings) in a @c(let*) form, then evaluate @c(forms) in
a @c(cond)."
  (let ((bindings (if (listp (first bindings)) bindings (list bindings))))
    `(let (,@bindings)
       (cond ,@forms))))

(defmacro! read-file-as-string (path &rest args &key (direction nil directionp)
					   &allow-other-keys)
  "Read the contents of the file at @c(path) as a string. Any
remaining arguments are sent to @c(with-open-file)."
  (when directionp
      (error "Specifying :direction is not allowed with READ-FILE-STRING."))
  `(with-open-file (g!s ,path ,@args)
     (let ((g!data (make-string (file-length g!s))))
       (read-sequence g!data g!s)
       g!data)))

(defmacro! with-string-output-to-file ((path &rest args &key (direction :output directionp)
				    &allow-other-keys)
				       &body body)
  "Evaluate @c(body), and call @c(mkstr) on the result. Write the
resulting string to @c(path). Any remaining arguments are sent to
@c(with-open-file)."
  (when directionp
    (error "Specifying :direction is not allowed with WRITE-FILE-STRING."))
  `(with-open-file (g!s ,path :direction ,direction ,@args)
     (with-standard-io-syntax
       (let ((o!out (mkstr ,@body)))
	 (princ o!out g!s)))))

(defmacro with-read-from-file ((stream path &rest args &key (direction :input directionp)
				       &allow-other-keys)
							 &body body)
  "Open @c(path) for reading, bound to @c(stream), with any remaining arguments
passed to @c(with-open-file), and execute @c(body)."
  (when directionp
    (error "Specifying :direction is not allowed with READ-FILE."))
  `(with-open-file (,stream ,path :direction ,direction ,@args)
     ,@body))

(defmacro with-write-to-file ((stream path &rest args
			      &key (direction :output directionp)
			      &allow-other-keys)
		      &body body)
  "Evaluate @c(body) with the file at @c(path) opened and bound to the
value of @c(stream). Any remaining keyword arguments are passed to
@c(with-open-file)."
  (when directionp
    (error "Specifying :direction is not allowed with WRITE-FILE."))
  `(with-open-file (,stream ,path :direction ,direction ,@args)
     ,@body))

(defun is (a b test key)
  "Are a and b equal? If not NIL, #'key is applied to b, then #'test
is called on both."
  (declare (inline))
  (funcall test
           (if key
               (funcall key b)
             b)
           a))

(defmacro in (obj seq &key (test #'eql) key deep)
  "Returns T if @c(obj), is in @c(seq).

If @c(seq) is a list, @c(test) is used to determine whether the object
matches. If @c(key) is not NIL, it is applied to elements before
@c(test). If @c(deep) is true, @c(seq) will be flattened before
checking the list.

If @c(seq) is a vector, @c(test) is used to determine whether the
object matches. If @c(key) is not NIL, it is applied to elements
before @c(test).

If @c(seq) is a hash table, @c(test) does not apply; the hash table's
test is used. If @c(key) is not NIL, it is applied to @c(obj) before
looking it up."
  (with-gensyms (obj% seq% key% x! found!)
    (declare (dynamic-extent obj% key% seq% x!))
    `(let ((,obj% ,obj)
	   (,key% ,key)
	   (,seq% (if ,deep ,(flatten seq) ,seq)))
       (cond
	 ((vectorp ,seq%)
	  (loop for ,x! across ,seq%
	     thereis (is ,obj% ,x! ,test ,key%)))
	 ((listp ,seq%)
	  (loop for ,x! in ,seq%
	     thereis (is ,obj% ,x! ,test ,key%)))
	 ((hash-table-p ,seq%)
	  (multiple-value-bind (,x! ,found!)
	      (gethash ,obj% ,seq%)
	    (declare (ignore ,x!))
	    ,found!))))))
