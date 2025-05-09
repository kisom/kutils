KUTILS

This is a collection of Kyle's utilities. They're things that I find
generally useful when writing programs.

There's a few groups of tools:

* Functions from On Lisp
* Functions and macros from Let Over Lambda
* Hash-table based functions
* Miscellaneous

On Lisp tools
=============

* mkstr
* symb
* group
* flatten
* compose

Let Over Lambda
===============

* defmacro!

Hash-table
==========

* hashkeys
* sethash
* hash-table-to-alist
* alist-to-hash-table

The function `enable-hashtable-reader` will activate a reader macro
for hash-tables using `:test #'equal`. This syntax looks like

    #{ k v ... }#

For example:

    CL-USER> (kutils:hashkeys (kutils:alist-to-hashtable (kutils:hashtable-to-alist #{:a :b :c :d}#)))
    (:A :C)

Miscellanea
===========

* interpose
* build-list
* partial
* macroexpand-n
* mksymb
* mkkw
* zip
* defclass!

Documentation
=============

Documentation is generated with codex [1]:

    CL-USER> (ql:quickload :codex) ;; if not installed
    (:CODEX)
    CL-USER> (codex:document :kutils)
    NIL
