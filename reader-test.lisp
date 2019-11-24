(defpackage :reader-test
  (:use :cl :prove :reader :alexandria))
(in-package :reader-test)

(plan nil)
(setq *default-reporter* :tap)

;; Owing to the use of let forms, try running tests after package reloading,
;; in case of failures.

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass foo () ((a :initform 3)))
  (defmethod get-val ((object foo) slot) (slot-value object slot))
  (defmethod (setf get-val) (new-value (object foo) slot)
    (setf (slot-value object slot) new-value))
  (defstruct bar a))


(named-readtables:in-readtable reader:reader)

(defmacro with-env (&body body)
  `(let ((str (copy-array "abcde"))
         (vec (copy-array #(a b c d e)))
         (list (copy-list '(a b c d e)))
         (ht {'a 'b 'c 'd})
         (arr (copy-array #2A((1 2) (3 4))))
         (assoc-list (copy-tree '((a . 1) (b . 2))))
         (plist (copy-list '(:a 4 :c 5)))
         (clos-object (make-instance 'foo))
         (struct (make-bar :a 3))
         (list-chained (copy-list (list 1 2 `#(a b ,(copy-array
                                                     #2A((q w e)(r t y)))))))
         (list-str (copy-list '("hello" "world")))
         (list-ht (list {"uniform" "utilities"})))
     ,@body))

(deftest get-val
  (with-env
    (is [str 0] #\a)
    (is [vec 0] 'a)
    (is [list 3] 'd)
    (is [ht 'a] 'b)
    (is [arr '(1 1)] 4)
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
    (is (progn (setf [arr '(0 1)] '(2))
               [arr '(0 1)])
        '(2)) ;; note: this is non-destructive!
    (is (progn (setf [clos-object 'a] 5)
               [clos-object 'a])
        5)
    (is (progn (setf [struct 'a] 7)
               [struct 'a])
        7)))

(with-env
    (deftest get-val-chained
      (is [list-chained 2 2 '(1 1)] 't)
      (is [list-str 1 3] #\l)
      (is [list-ht 0 "uniform"] "utilities" :test #'string=)))

(with-env
    (deftest setf-get-val-chained
      (is (progn (setf [list-chained 2 2 '(1 1)]
                       list-ht)
                 [list-chained 2 2 '(1 1) 0 "uniform"])
          "utilities"
          :test #'string=)))

(deftest lambda-reader-macro
  (is-type λ() 'function)
  (is (λ(+ - --) 2 3) 5)
  (is (λ(list -) 'a) '(a) :test #'equalp)
  (is (λ(* (+ - --) ---) 2 3 4)
      20)
  (is (λ3(null args) 1 2 3) t))

(deftest mapcar-reader-macro
  (is #[λ(write-to-string -) '(2 5 a)]
      '("2" "5" "A")))

(deftest hash-set-reader-macro
  (let ((hash-set #{'a 'b 1}))
    (is-type hash-set 'hash-set:hash-set)
    (is-type (hash-set:hs-memberp hash-set 'a) t)
    (is-type (hash-set:hs-memberp hash-set 'b) t)
    (is-type (hash-set:hs-memberp hash-set 1) t)
    (is-type (hash-set:hs-memberp hash-set 2) 'null)))


