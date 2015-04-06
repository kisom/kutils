(in-package #:kutils)

;;;; Utilities from Let Over Lambda. This source code is provided
;;;; under the following license:


(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"G!"
		:start1 0
		:end1   2)))

(defmacro defmacro/g! (name args &rest body)
  "defmacro/g! provides automatic gensyms for all arguments starting
with g!."
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
	      (lambda (s)
		`(,s (gensym ,(subseq
			       (symbol-name s)
			       2))))
	      syms)
	 ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
		"O!"
		:start1 0
		:end1   2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!" (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  "defmacro! provides automatic gensyms and once-only
evaluation. Arguments that begin with g! will be automatically
gensym'd, and arguments that begin with o! will only be evaluated
once. Inside the body, the o! arguments should be called as their
equivalent g! argument: o!x should be called in the body as g!x."
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))
