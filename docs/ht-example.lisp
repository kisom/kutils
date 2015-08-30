* (with-new-hash-table (ht)
    (sethash "case" '("neuromancer") ht)
    (sethash "bigend" '("pattern recognition"
			"spook country"
			"zero history")
	     ht))
#<HASH-TABLE :TEST EQUAL :COUNT 2 {1009044DC3}>

* (with-new-hash-table (neuromancer pattern-recognition)
    (sethash "places"
	     '("chiba city"
	       "istanbul"
	       "villa straylight")
	     neuromancer)
    (sethash "places"
	     '("london"
	       "tokyo"
	       "moscow")
	     pattern-recognition))
((NEUROMANCER . #<HASH-TABLE :TEST EQUAL :COUNT 1 {100959E273}>)
 (PATTERN-RECOGNITION . #<HASH-TABLE :TEST EQUAL :COUNT 1 {100959E6B3}>))
