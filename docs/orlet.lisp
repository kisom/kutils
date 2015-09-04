(macroexpand-1
	  '(orlet ((cls  (class-symbol s))
		   (slots (only-key-slots cls))
		   (args (json-slots tbl slots)))
	    (apply #'make-instance cls args)))
(LET ((CLD (CLASS-SYMBOL S)))
  (WHEN CLS
    (LET ((SLOTS (ONLY-KEY-SLOTS CLS)))
      (WHEN SLOTS
	(LET ((ARGS (JSON-SLOTS TBL SLOTS)))
	  (WHEN ARGS
	    (PROGN (APPLY #'MAKE-INSTANCE CLS ARGS))))))))
