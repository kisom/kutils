* (macroexpand-1
   '(whenlet ((kwarg (slot-initarg slot))
	      (value (dispatch-get-value
		      slot
		      (slot-table-value
		       slot ht snake-case))))
     (list kwarg value)))
(LET ((KWARG (SLOT-INITARG SLOT))
      (VALUE
       (DISPATCH-GET-VALUE SLOT
			   (SLOT-TABLE-VALUE SLOT HT SNAKE-CASE))))
  (WHEN (AND KWARG VALUE)
    (LIST KWARG VALUE)))
