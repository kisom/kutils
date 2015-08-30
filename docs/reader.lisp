CL-USER> (gethash :foo  #{:foo :bar :baz :quux}#)
:BAR
T
CL-USER> (let ((m #{:foo :bar :baz :quux}#))
           (sethash :baz :woz m)
           (hashkeys m))
(:BAZ :FOO)
CL-USER> (hashtable-to-alist #{:a :b :c :d}#)
((:C . :D) (:A . :B))
