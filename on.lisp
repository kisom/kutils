(in-package #:kutils)

;;;; The utilities in this file come from the book On Lisp by Paul
;;;; Graham.

(defun mkstr (&rest args)
  "Concatenates its symbols and returns the printable representation
of the result."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Passes its arguments to mkstr to produce a printable
representation, and returns the symbol built from this result; if the
symbol does not exist, it will be created."
  (values (intern (apply #'mkstr args))))

(defun group (source n)
  "Group takes a list as input and produces a list of sublists of
length n."
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons
			      (subseq source 0 n)
			      acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Returns a list of all atoms present in the provided list. Of
historical note, this was originally provided in Interlisp."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x)
			   (rec (cdr x) acc))))))
    (rec x nil)))

;;; compose may have been one of the first functions that really made
;;; me pay attention to Lisp. I'm not sure why, particularly, but it
;;; helped spur further Lisp explorations.n

(defun compose (&rest fns)
  "Compose allows a number of functions with the same arity to be
composed together in a chain."
      (if fns
          (let ((fn1 (car (last fns)))
                (fns (butlast fns)))
            #'(lambda (&rest args)
                (reduce #'funcall fns
                        :from-end t
                        :initial-value (apply fn1 args))))
          #'identity))

(defmacro with-gensyms (syms &body body)
  "Binds a whole list of variables to gensyms."
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))


