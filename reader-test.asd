(defsystem :reader-test
    :author "Shubhamkar B. Ayare"
    :license "MIT"
    :depends-on (:reader :iterate :alexandria)
    :components ((:file "reader-test-framework")
                 (:file "reader-test"))
    :description "Test system for reader") 
