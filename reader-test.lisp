(defpackage :reader-test
  (:use :cl :prove :reader :alexandria))
(in-package :reader-test)

(plan nil)
(setq *default-reporter* :tap)

;; Owing to the use of let forms, try running tests after package reloading,
;; in case of failures.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass foo () ((a :initform 3)))
  (defmethod get-val ((object foo) &rest slot/s) (slot-value object (car slot/s)))
  (defmethod (setf get-val) (new-value (object foo) &rest slot/s)
    (setf (slot-value object (car slot/s)) new-value))
  (defstruct bar a))


(named-readtables:in-readtable reader:reader)

(defmacro with-env (&body body)
  `(let ((str (copy-array "abcde"))
         (vec (copy-array #(a b c d e)))
         (list (copy-list '(a b c d e)))
         (ht {'a 'b 'c 'd})
         (assoc-list (copy-tree '((a . 1) (b . 2))))
         (plist (copy-list '(:a 4 :c 5)))
         (clos-object (make-instance 'foo))
         (struct (make-bar :a 3)))
     ,@body))

(deftest get-val
  (with-env
    (is [str 0] #\a)
    (is [vec 0] 'a)
    (is [list 3] 'd)
    (is [ht 'a] 'b)
    (is [assoc-list 'a] 1)
    (is [assoc-list 'c] nil)
    (is [plist :c] 5)
    (is [plist :e] nil)
    (is [clos-object 'a] 3)
    (is [struct 'a] 3)))

(deftest setf-get-val
  (with-env
    (is (progn (setf [str 0] #\f)
               [str 0])
        #\f)
    (is (progn (setf [vec 0] 'f)
               [vec 0])
        'f)
    (is (let ()
          (setf [list 0] 'f)
          [list 0])
        'f)
    (is (progn (setf [ht 'a] 'f)
               [ht 'a])
        'f)
    (is (progn (setf [clos-object 'a] 5)
               [clos-object 'a])
        5)
    (is (progn (setf [struct 'a] 7)
               [struct 'a])
        7)))

(deftest get-val-numcl
  (let* ((array #2A((1 2 3) (4 5 6)))
         (num-array (numcl:asarray array)))
    (is [array 0 0] 1)
    (is [num-array t 0]
        #(1 4)
        :test 'equalp)))

(deftest setf-get-val-numcl
  (let* ((array #2A((1 2 3) (4 5 6)))
         (num-array (numcl:asarray array)))
    (setf [array 0 0] 0)
    (is [array 0 0] 0)
    (setf [num-array t 0] (numcl:zeros 2))
    (is [num-array t 0]
        #(0 0)
        :test 'equalp)))

(deftest lambda-reader-macro
  (is-type λ() 'function)
  (is (λ(+ - --) 2 3) 5)
  (is (λ(list -) 'a) '(a) :test #'equalp)
  (is (λ(* (+ - --) ---) 2 3 4)
      20)
  (is (λ3(null args) 1 2 3) t))

(deftest map-reader-macro
  (is #[λ(write-to-string -) '(2 5 a)]
      '("2" "5" "A"))
  (is #[λ(+ - --) #(1 2 3) #(2 3 4)]
      #(3 5 7)
      :test 'equalp))

(deftest hash-set-reader-macro
  (let ((hash-set #{'a 'b 1}))
    (is-type hash-set 'hash-set:hash-set)
    (is-type (hash-set:hs-memberp hash-set 'a) t)
    (is-type (hash-set:hs-memberp hash-set 'b) t)
    (is-type (hash-set:hs-memberp hash-set 1) t)
    (is-type (hash-set:hs-memberp hash-set 2) 'null)))


