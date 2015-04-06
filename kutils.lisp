;;;; kutils.lisp

(in-package #:kutils)

;;; "kutil" goes here. Hacks and glory await!

(defun join (x sep)
  (flatten
   (mapcar (lambda (y)
	     (list y sep))
	   x)))


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
		   (progn
		     (setq key-p nil
			   k     (read-from-string (finalise-read k))))
		   (progn
		     (setq key-p t
			   v     (read-from-string (finalise-read v)))
		     (setf (gethash k m) v)
		     (setq k nil v nil)))))
      (do ((prev (read-char stream) curr)
	   (curr (read-char stream) (read-char stream)))
	  ((and (char= prev #\}) (char= curr #\#)))
	(if (char= prev #\Space)
	    (finalise-kv-pair)
	    (if key-p
		(push prev k)
		(push prev v))))
      (if (and (null k)
	       (null v))
	  (error "Mismatched key value pairs.")
	  (progn 
	    (finalise-kv-pair)
	    m)))))

(set-dispatch-macro-character
 #\# #\{ #'|#{-reader|)

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

(defun hashtable-to-alist (m)
  (let ((alist '()))
    (maphash (lambda (k v)
	       (let ((elt (cons k v)))
		 (setf alist (cons elt alist))))
	     m)
    alist))
