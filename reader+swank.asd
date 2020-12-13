(asdf:defsystem "reader+swank"
  :description "Integration with SWANK using SWANK:*READTABLE-ALIST*."
  :depends-on ("reader"
               #-swank "swank")
  :components ((:file "swank")))
