(in-package #:kutils)

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
  "Enables the reader macro @c(#{}#) for hash-tables. The resulting
hash-table will use @c(#'equal) for equality. For example,

@begin[lang=lisp](code)
#{:a :b :c :d}#
@end(code)

will create a hash-table with the keys @c(:a) and @c(:c); :@c(a)
stores the value @c(:b), and @c(:c) stores the value @c(:d).
"
  (set-dispatch-macro-character
   #\# #\{ #'|#{-reader|))

(defmacro sethash (k v ht)
  "Convenience notation for setting a value in a hash table."
  `(setf (gethash ,k ,ht) ,v))

(defun hashkeys (ht)
  "Returns a list of the keys in a hash table."
  (let ((keys '()))
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k keys))
	     ht)
    keys))

(defun hash-table-to-alist (ht)
  "Converts the hash-table argument to an alist of @c((key . value))
pairs."
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (setf alist (cons elt alist))))
	     ht)
    alist))


(defun new-hash-table ()
  "Create a new hash table with the @c(#'equal) function as its test."
  (make-hash-table :test #'equal))

(defmacro with-new-hash-table (htsyms &body body)
  "Create and bind a new hash table for each of the symbols in @c(htsyms),
executing inside a let form, and returns the hash table(s). If only
one hash table is provided, return it as a single element; otherwise,
return an alist of the symbol names and hash tables."
  `(let ,(mapcar (lambda (sym)
                   (list sym (list 'new-hash-table))) htsyms)
     ,@body
     ,(if (null (rest htsyms))
	  (first htsyms)
	  `(mapcar #'cons
		  (mapcar #'mksymb (quote ,htsyms))
		  (list ,@htsyms)))))

(defun alist-to-hash-table (alist)
  "Converts the alist to a hash-table."
  (with-new-hash-table (ht)
    (dolist (elt alist)
      (sethash (car elt) (cdr elt) ht))))

(defun copy-hash-table (ht)
  "Shallow copy @c(ht)."
  (with-new-hash-table (copied)
    (maphash (lambda (k v)
	       (sethash k v copied))
	     ht)))

