(asdf:defsystem "reader"
  :serial t
  :license "MIT"
  :version "0.11.0" ;; beta
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :description "A utility library intended at providing configurable reader macros for
common tasks such as accessors, hash-tables, sets, uiop:run-program, arrays and a few others."
  :depends-on ("alexandria"
               "fiveam"
               "iterate"
               "split-sequence"
               "trivial-types"
               "uiop")
  :components ((:file "reader"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                       (5AM:RUN! :READER))"))))

