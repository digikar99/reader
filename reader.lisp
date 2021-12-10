(defpackage :reader
  (:use :cl :iterate :split-sequence)
  (:import-from :fiveam :is :is-true :is-false)
  (:export
   :args
   :get-val
   :enable-reader-syntax
   :disable-reader-syntax
   :*array-function*
   :*hash-table-function*
   :*get-val-function*
   :*get-val-array-function*
   :*alists-are-lists*
   :*plists-are-lists*
   :*set-function*))

(in-package :reader)
(5am:def-suite :reader)
(5am:in-suite :reader)

;;;;;;;;;;;;;;;;;;;;;;;; CONFIGURATION VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *array-function*
  (defun make-t-array-from-initial-contents (initial-contents)
    (labels ((dimensions (contents)
               (if (listp contents)
                   (cons (length contents)
                         (dimensions (car contents)))
                   ())))
      (cl:make-array (dimensions initial-contents)
                     :element-type t
                     :initial-contents initial-contents)))
  "A symbol bound (at read-time) to a function that takes INITIAL-CONTENTS
as its argument and returns the array. INITIAL-CONTENTS is a list of nested-lists.")

(defparameter *hash-table-function* 'alexandria:plist-hash-table
  "A symbol bound (at read-time) to a function that takes
  (PLIST &KEY TEST)
as arguments and returns the hash-table.")
(defparameter *get-val-function* 'get-val
  "A symbol bound (at read-time) to a function that takes
  (OBJECT &REST KEY/S)
as arguments and returns the value corresponding to the KEY/S.")
(defparameter *get-val-array-function* 'cl:aref
  "A symbol bound (at read-time) to a function that takes
  (ARRAY &REST SUBSCRIPTS)
as arguments and returns the corresponding value.
This is assumed to have a SETF defined.
This variable is significant if READER:*GET-VAL-FUNCTION* is bound to READER:GET-VAL.")

(defparameter *set-function*
  'cl:remove-duplicates
  "A symbol bound (at read-time) to a function that takes
  (LIST &KEY TEST)
as arguments and returns the set object.")

(defparameter *alists-are-lists* nil
  "If T, ALISTS will not be treated specially by READER:GET-VAL method for lists.")
(defparameter *plists-are-lists* nil
  "If T, PLISTS will not be treated specially by READER:GET-VAL method for lists.")

(defmacro with-reader-syntax (reader-macro-identifiers &body body)
  "This macro is only made for use by read-and-eval functionality,
and rather directed towards tests than users. So, do not export."
  (let ((identifiers (loop :for identifier :in reader-macro-identifiers
                           :collect `(quote ,identifier))))
    `(let ((*array-function*         'make-t-array-from-initial-contents)
           (*hash-table-function*    'alexandria:plist-hash-table)
           (*get-val-function*       'get-val)
           (*get-val-array-function* 'cl:aref)
           (*set-function*           'cl:remove-duplicates)
           (*alists-are-lists*       nil)
           (*plists-are-lists*       nil)
           (*readtable*             (%enable-reader-syntax *readtable* ,@identifiers)))
       ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;; READTABLE HANDLERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *previous-readtables* nil)

(defparameter *reader-macro-activation-functions*
  (list (cons "GET-VAL" (lambda ()
                          (set-macro-character #\[ 'get-val-reader-macro)
                          (set-macro-character #\] (lambda (stream char)
                                                     (declare (ignore stream char))
                                                     (error "No matching [ for ]")))))
        (cons "HASH-TABLE"  (lambda ()
                              (set-macro-character #\{ 'hash-table-reader-macro)
                              (set-macro-character #\}
                                                   (lambda (stream char)
                                                     (declare (ignore stream char))
                                                     (error "No matching { for }")))))
        (cons "NOT" (lambda () (set-macro-character #\! 'not-reader-macro)))
        (cons "STRING" (lambda () (set-macro-character #\$ 'string-reader-macro)))
        (cons "DESCRIBE" (lambda () (set-macro-character #\? 'describe-reader-macro)))
        (cons "ARRAY" (lambda ()
                        (set-dispatch-macro-character #\# #\[ 'array-reader-macro)
                        (set-macro-character #\] (lambda (stream char)
                                                   (declare (ignore stream char))
                                                   (error "No matching [ for ]")))))
        (cons "SET" (lambda ()
                      (set-dispatch-macro-character #\# #\{
                                                    'set-reader-macro)
                      (set-macro-character #\}
                                           (lambda (stream char)
                                             (declare (ignore stream char))
                                             (error "No matching { for }")))))
        (cons "RUN-PROGRAM"
              (lambda ()
                (set-dispatch-macro-character #\# #\!
                                              'run-program-reader-macro)))))

(alexandria:define-constant +reader-macro-doc+
  "READER-MACRO-IDENTIFIERS are any of the following symbols:
  GET-VAL, HASH-TABLE, NOT, STRING, DESCRIBE, ARRAY, SET, RUN-PROGRAM"
  :test 'string=)

(defun %enable-reader-syntax (readtable &rest reader-macro-identifiers)
  (let ((*readtable* readtable))
    (mapcar (lambda (identifier)
              (funcall (get-val *reader-macro-activation-functions*
                                identifier
                                :test #'string=)))
            reader-macro-identifiers)
    *readtable*))

(defmacro enable-reader-syntax (&rest reader-macro-identifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setq *readtable* (copy-readtable))
     (%enable-reader-syntax *readtable* ,@reader-macro-identifiers)))

(defmacro disable-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* (if *previous-readtables*
                           (pop *previous-readtables*)
                           (copy-readtable nil)))))

(setf (documentation 'enable-reader-syntax 'function) +reader-macro-doc+)

(defun er (string) (eval (read-from-string string)))
(defmacro setp (new-value place)
  `(progn
     (setf ,place ,new-value)
     ,place))

(defun read-stream-until (stream until-char)
  (iter (for next-char = (peek-char t stream nil))
    (unless next-char (next-iteration))
    (until (char= until-char next-char))
    (collect (read stream nil))
    (finally (read-char stream nil))))

;;;;;;;;;;;;;;;;;;;;;;;; ARRAY READER MACRO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-stream-as-string-until (stream until-char)
  (iter (for next-char = (peek-char nil stream nil))
    (while next-char)
    (until (char= until-char next-char))
    (collect (read-char stream nil)
      into string result-type 'string)
    (finally (read-char stream nil)
             (return string))))
(defun read-row-from-string (string)
  (with-input-from-string (str string)
    (iter (for obj = (read str nil))
      (while obj)
      (collect obj))))
(defun read-array (stream)
  (let (array-dimensions)
    (iter (for next-char = (peek-char t stream nil))
      (alexandria:switch (next-char)
        (#\[ (read-char stream)
             (collect (multiple-value-bind (array dimensions) (read-array stream)
                        (setq array-dimensions dimensions)
                        array)
               into list))
        (#\] (read-char stream)
             (setq array-dimensions
                   (cons (length list) array-dimensions))
             (return-from read-array (values (cons 'list list)
                                             array-dimensions)))
        (t (let* ((element-list (split-sequence #\newline
                                                (read-stream-as-string-until stream #\])
                                                :remove-empty-subseqs t))
                  (num-rows (length element-list))
                  (num-cols (length (read-row-from-string (first element-list)))))
             (return-from read-array
               (cond ((null (rest element-list))
                      (values (cons 'list (read-row-from-string (first element-list)))
                              (list num-cols)))
                     (t (values (cons 'list (mapcar (lambda (list) (cons 'list list))
                                                    (mapcar #'read-row-from-string element-list)))
                                (list num-rows num-cols)))))))))))

(defun array-reader-macro (stream char n)
  (declare (ignore char n))
  (multiple-value-bind (initial-contents array-dimensions) (read-array stream)
    (declare (ignorable array-dimensions))
    `(,*array-function* ,initial-contents)))

(5am:def-test array ()
  (with-reader-syntax (array)
    (is (equalp #(1 2) (eval (read-from-string "#[1 2]"))))
    (is (equalp #2A((1 2 3) (4 5 6))
                (eval (read-from-string "#[1 2 3
                                           4 5 6]"))))
    (is (equalp #2A((1 2 3) (4 5 6))
                (eval (read-from-string "#[[1 2 3] [4 5 6]]"))))
    (is (equalp #(1 2)
                (eval (read-from-string "(let ((a 1) (b 2)) #[a b])"))))))

(defmacro defmethods-with-setf (fun-name lambda-list &rest methods)
  `(progn
     ,@(let ((new-value (gensym)))
         `(,(let ((methods methods))
              `(defgeneric ,fun-name ,lambda-list
                 ,@(loop for args = (first methods)
                         for body = (second methods)
                         do (setq methods (cddr methods))
                         while methods
                         collect `(:method ,args
                                    (assert (= 1 (length key/s)))
                                    ,body))))
           ,@(let ((methods methods))
               (loop for args = (first methods)
                     for body = (second methods)
                     while methods
                     collect `(defmethod (setf ,fun-name) (,new-value ,@args)
                                (setf ,body ,new-value))
                     do (setq methods (cddr methods))))))))

#+sbcl (declaim (sb-ext:maybe-inline get-val))

(defmethods-with-setf get-val (object &rest key/s)
  ((object hash-table) &rest key/s) (gethash (car key/s) object)
  ((object sequence) &rest key/s) (elt object (car key/s))
  ((object structure-object) &rest key/s) (slot-value object (car key/s))
  ((object standard-object) &rest key/s) (slot-value object (car key/s)))

;;; An attempt may be made using specializing on trivial-types:association-list
;;; However, an error that this is not a class is raised.
;;; Further, we also need to allow the use of ['(a b c d) 3] => d rather
;;; than nil.

(defmethod get-val ((object list) &rest key/s)
  (let ((key (car key/s)))
    (cond ((and (not *alists-are-lists*)
                (trivial-types:association-list-p object))
           (let ((pair (apply #'assoc key object (rest key/s))))
             (if pair (cdr pair) nil)))
          (t
           (assert (null (rest key/s)))
           (if (and (not *plists-are-lists*)
                    (trivial-types:property-list-p object)
                    (not (integerp key)))
               (getf object key)
               (nth key object))))))

;; TODO: SETF expansions with generic-functions to set object when bound to null?
(defmethod (setf get-val) (new-value (object list) &rest key/s)
  (assert (not (null object)))
  (let ((key (car key/s)))
    (cond ((and (not *alists-are-lists*)
                (trivial-types:association-list-p object))
           (let ((pair (apply #'assoc key object (rest key/s))))
             (if pair
                 (setf (cdr pair) new-value)
                 (setf (cdr object)
                       (cons (cons key new-value)
                             (cdr object))))))
          (t
           (assert (null (rest key/s)))
           (if (and (not *plists-are-lists*)
                    (trivial-types:property-list-p object)
                    (not (integerp key)))
               (let ((value (getf object key)))
                 (if value
                     (setf (getf object key) new-value)
                     (setf (cddr object)
                           (cons key (cons new-value (cddr object))))))
               (setf (nth key object) new-value))))))

(defmethod get-val ((object array) &rest key/s)
  (apply *get-val-array-function* object key/s))

(defmethod (setf get-val) (new-value (object array) &rest key/s)
  (case *get-val-array-function*
    (aref (setf (apply #'aref object key/s) new-value))
    (t    (apply (fdefinition (list 'setf *get-val-array-function*))
                 new-value object key/s))))

;; (defmethod (setf get-val) (new-value (object array) &rest key/s)
;;   (setf (apply *get-val-array-function* object key/s) new-value))

;;;;;;;;;;;;;;;;;;;;;;;; the remaining reader macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-val-reader-macro (stream char)
  (declare (ignore char))
  `(,*get-val-function* ,@(read-stream-until stream #\])))

(defmacro with-env (&body body)
  (let ((body-value (gensym)))
    `(let (,body-value)
       (defclass foo () ((a :initform 3)))
       (defmethod get-val ((object foo) &rest slot/s) (slot-value object (car slot/s)))
       (defmethod (setf get-val) (new-value (object foo) &rest slot/s)
         (setf (slot-value object (car slot/s)) new-value))
       (defstruct bar a)
       (setq ,body-value
             (let ((str (alexandria:copy-array "abcde"))
                   (vec (alexandria:copy-array #(a b c d e)))
                   (arr (alexandria:copy-array #2A((1 2 3)
                                                   (4 5 6))))
                   (list      (copy-list '(a b c d e)))
                   (ht        (eval (read-from-string "{'a 'b 'c 'd}")))
                   (ht-eq     (eval (read-from-string "{eq \"a\" 1
                                                    \"b\" 2}")))
                   (ht-equalp (eval (read-from-string "{equalp '(1 2 3) \"a\"
                                                               '(4 5 6) \"b\"}")))
                   (assoc-list (copy-tree '((a . 1) (b . 2))))
                   (plist      (copy-list '(:a 4 :c 5)))
                   (clos-object (make-instance 'foo))
                   (struct      (make-bar :a 3)))
               (declare (ignorable str vec arr list ht ht-eq ht-equalp
                                   assoc-list plist clos-object struct))
               ,@body))
       (setf (find-class 'foo nil) nil)
       (setf (find-class 'bar nil) nil)
       ,body-value)))

(5am:def-test get-val (:depends-on (and array hash-table))
  (with-reader-syntax (get-val array hash-table)
    (with-output-to-string (*error-output*)
      (is (char= #\a  (er "(with-env [str 0])")))
      (is (eq    'a   (er "(with-env [vec 0])")))
      (is (=     3    (er "(with-env [arr 0 2])")))
      (is (eq    'd   (er "(with-env [list 3])")))
      (is (eq    'b   (er "(with-env [ht 'a])")))
      (is (eq    nil  (er "(with-env [ht-eq (string (alexandria:copy-array \"a\"))])")))
      (is (equal "b"  (er "(with-env [ht-equalp (copy-list '(4 5 6))])")))
      (is (=     1    (er "(with-env [assoc-list 'a])")))
      (is (eq    nil  (er "(with-env [assoc-list 'c])")))
      (is (=     5    (er "(with-env [plist :c])")))
      (is (eq    nil  (er "(with-env [plist :e])")))
      (is (eq    3    (er "(with-env [clos-object 'a])")))
      (is (eq    3    (er "(with-env [struct 'a])"))))))

(5am:def-test setf-get-val (:depends-on (and array hash-table))
  (with-reader-syntax (get-val array hash-table)
    (with-output-to-string (*error-output*)
      (is (char= #\f  (er "(with-env (setp #\\f [str 0]))")))
      (is (eq    'f   (er "(with-env (setp 'f   [vec 0]))")))
      (is (=     4    (er "(with-env (setp 4    [arr 0 2]))")))
      (is (eq    'f   (er "(with-env (setp 'f   [list 3]))")))
      (is (eq    'f   (er "(with-env (setp 'f   [ht 'a]))")))
      (is (=     2    (er "(with-env (setp 2    [assoc-list 'a]))")))
      (is (eq    3    (er "(with-env (setp 3    [assoc-list 'c]))")))
      (is (=     2    (er "(with-env (setp 2    [plist :c]))")))
      (is (eq    3    (er "(with-env (setp 3    [plist :e]))")))
      (is (eq    5    (er "(with-env (setp 5    [clos-object 'a]))")))
      (is (eq    7    (er "(with-env (setp 7    [struct 'a]))"))))))

(defun hash-table-reader-macro (stream char)
  (declare (ignore char))
  (let* ((inputs (read-stream-until stream #\}))
         (len    (length inputs))
         (test   (if (oddp len)
                     (first inputs)
                     'equalp))
         (inputs (if (oddp len)
                     (rest inputs)
                     inputs)))
    `(,*hash-table-function* (list ,@inputs) :test ',test)))

(5am:def-test hash-table ()
  (with-reader-syntax (hash-table)
    (is (equalp (alexandria:plist-hash-table '(:a 1 :b 2) :test 'equalp)
                (er "{:a 1 :b 2}")))
    (is (equalp (alexandria:plist-hash-table '(:a 1 :b 2) :test 'eq)
                (er "{eq :a 1 :b 2}")))
    (is (equalp (alexandria:plist-hash-table '("a" 1.0 "b" 2) :test 'equalp)
                (er "{\"a\" 1 \"b\" 2}")))))

(defun set-reader-macro (stream char n)
  (declare (ignore char n))
  (let* ((items (iter (until (char= (peek-char t stream nil)
                                    #\}))
                  (collect (read stream))
                  (finally (read-char stream nil))))
         (num-items (length items)))
    (cond ((< num-items 2)
           `(,*set-function* (list ,@items)))
          ((eq :test (car (last items 2)))
           `(,*set-function* (list ,@(butlast items 2))
                             ,@(last items 2)))
          (t
           `(,*set-function* (list ,@items))))))

(5am:def-test set ()
  (with-reader-syntax (set)
    (let ((set (er "#{'a 'b 1 '(1) '(1) :test #'equal}")))
      (is-true  (alexandria:setp set))
      (is-true  (member 'a set))
      (is-true  (member 'b set))
      (is-true  (member 1  set))
      (is-false (member 2  set))
      (is (= 1 (count '(1) set :test #'equal))))))

(defun run-program-reader-macro (stream char n)
  (declare (ignore char n))
  `(uiop:run-program ,(read-line stream) :output t))

(defun not-reader-macro (stream char)
  (declare (ignore char))
  `(not ,(read stream)))

(defun string-reader-macro (stream char)
  (declare (ignore char))
  `(write-to-string ,(read stream)))

(defun describe-reader-macro (stream char)
  (declare (ignore char))
  `(describe (quote ,(read stream))))

(5am:def-test run-program ()
  (with-reader-syntax (run-program)-
    (is (string= "hello world"
                 (er "(with-output-to-string (*standard-output*)
                        #!echo -n hello world
                        )")))))

(5am:def-test string ()
  (with-reader-syntax (string)
    (is (string-equal "5.0d0" (er "(let ((a 5.0d0)) $a)")))))

(5am:def-test describe ()
  (with-reader-syntax (describe)
    (is (string= (with-output-to-string (*standard-output*)
                   (describe 'reader:enable-reader-syntax))
                 (er "(with-output-to-string (*standard-output*)
                         ?reader:enable-reader-syntax
                         )")))))
