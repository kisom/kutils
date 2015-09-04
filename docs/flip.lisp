CL-USER> (format t "(- 7 3) -> ~A~%"
		 (funcall (flip #'- 3) 7))
(- 7 3) -> 4
NIL

