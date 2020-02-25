(asdf:defsystem :reader
  :serial t
  :license "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A utility library intended at providing reader macros for lambdas, arrays, accessors, hash-tables and hash-sets."
  :depends-on ("iterate"
               "named-readtables"
               "hash-set"
               "alexandria"
               "trivial-types"
               "numcl"
               "str")
  :components ((:file "reader")))


