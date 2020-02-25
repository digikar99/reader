(defpackage :reader-test
  (:use :cl :reader-test-framework :alexandria :reader)
  (:export :run))
(in-package :reader-test)

(defun run ()
  (run-test-package :reader-test))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defclass foo () ((a :initform 3)))
  (defmethod get-val ((object foo) &rest slot/s) (slot-value object (car slot/s)))
  (defmethod (setf get-val) (new-value (object foo) &rest slot/s)
    (setf (slot-value object (car slot/s)) new-value))
  (defstruct bar a))

(reader:enable-reader-syntax 'get-val 'hash-table)

(defmacro with-env (&body body)
  `(let ((str (copy-array "abcde"))
         (vec (copy-array #(a b c d e)))
         (list (copy-list '(a b c d e)))
         (ht {'a 'b 'c 'd})
         (ht-eq {:eq "a" 1
                     "b" 2})
         (ht-equalp {:equalp '(1 2 3) "a"
                             '(4 5 6) "b"})
         (assoc-list (copy-tree '((a . 1) (b . 2))))
         (plist (copy-list '(:a 4 :c 5)))
         (clos-object (make-instance 'foo))
         (struct (make-bar :a 3)))
     ,@body))

(define-test get-val
  (with-env
    (is [str 0] #\a)
    (is [vec 0] 'a)
    (is [list 3] 'd)
    (is [ht 'a] 'b)
    (is [ht-eq (string (copy-array "a"))] nil)
    (is [ht-equalp (copy-list '(4 5 6))] "b" :test 'equal)
    (is [assoc-list 'a] 1)
    (is [assoc-list 'c] nil)
    (is [plist :c] 5)
    (is [plist :e] nil)
    (is [clos-object 'a] 3)
    (is [struct 'a] 3)))

(define-test setf-get-val
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

(define-test get-val-numcl
  (let* ((array #2A((1 2 3) (4 5 6)))
         (num-array (numcl:asarray array)))
    (is [array 0 0] 1)
    (is [num-array t 0]
        #(1 4)
        :test 'equalp)))

(define-test setf-get-val-numcl
  (let* ((array #2A((1 2 3) (4 5 6)))
         (num-array (numcl:asarray array)))
    (setf [array 0 0] 0)
    (is [array 0 0] 0)
    (setf [num-array t 0] (numcl:zeros 2))
    (is [num-array t 0]
        #(0 0)
        :test 'equalp)))

(reader:disable-reader-syntax)
(reader:enable-reader-syntax 'lambda 'array 'hash-set)

(define-test lambda-reader-macro
  (is-type λ() 'function)
  (is (λ(+ - --) 2 3) 5)
  (is (λ(list -) 'a) '(a) :test #'equalp)
  (is (λ(* (+ - --) ---) 2 3 4)
      20)
  (is (λ3(null args) 1 2 3) t))

(define-test array-reader-macro
  (is (let ((a 3) (b 5) (c 7)) #[a b c])
      #(3 5 7)
      :test 'equalp)
  (is (let ((a 3) (b 5) (c 7)) #[[a b c
                                  c b a]
                                 [a b c
                                  c b a]])
      #3A(((3 5 7) (7 5 3)) ((3 5 7) (7 5 3)))
      :test 'equalp))

(define-test hash-set-reader-macro
  (let ((hash-set #{'a 'b 1}))
    (is-type hash-set 'hash-set:hash-set)
    (is-type (hash-set:hs-memberp hash-set 'a) t)
    (is-type (hash-set:hs-memberp hash-set 'b) t)
    (is-type (hash-set:hs-memberp hash-set 1) t)
    (is-type (hash-set:hs-memberp hash-set 2) 'null)))

(reader:disable-reader-syntax)
