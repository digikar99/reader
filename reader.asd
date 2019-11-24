(asdf:defsystem :reader
  :serial t
  :license "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A utility library intended at providing reader macros for lambdas, mapping, accessors, hash-tables and hash-sets."
  :depends-on ("iterate"
               "named-readtables"
               "hash-set"
               "alexandria"
               "trivial-types"
               "numcl"
               "generic-cl")
  :components ((:file "reader")))


