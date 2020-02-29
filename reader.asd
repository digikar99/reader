(asdf:defsystem :reader
  :serial t
  :license "MIT"
  :version "0.9.0" ;; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A utility library intended at providing reader macros for lambdas, arrays, accessors, hash-tables and hash-sets."
  :depends-on ("iterate"
               "named-readtables"
               "hash-set"
               "alexandria"
               "trivial-types"
               "select"
               "str" ;; str uses cl-interpol; so no overhead!
               "cl-interpol")
  :components ((:file "reader")))


