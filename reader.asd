(asdf:defsystem :reader
  :serial t
  :license "MIT"
  :version "0.9.2" ;; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A utility library intended at providing reader macros for lambda, get-val, hash-table, not, string, describe, array, hash-set and run-program."
  :depends-on ("iterate"
               "named-readtables"
               "hash-set"
               "alexandria"
               "trivial-types"
               "select"
               "str"
               "uiop"
               "numcl")
  :components ((:file "reader")))

