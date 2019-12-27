(defpackage :reader
  (:use :named-readtables :cl :iterate :hash-set)
  (:export
   :-
   :--
   :---
   :args
   :reader
   :get-val
   :enable-reader-syntax
   :disable-reader-syntax))

(in-package :reader)

(defvar *previous-readtables* nil)

(defun %enable-reader-syntax (&rest reader-macro-identifiers)
  "READER-MACRO-IDENTIFIERS are any of the following symbols:
  LAMBDA, GET-VAL, HASH-TABLE, MAP, HASH-SET"
  (let ((reader-macro-identifier-strings (mapcar #'symbol-name
                                                 reader-macro-identifiers))
        (reader-macro-activation-functions
         (list (cons "LAMBDA" (lambda ()
                                (set-macro-character #\GREEK_SMALL_LETTER_LAMDA
                                                     'lambda-reader-macro)))
               (cons "GET-VAL" (lambda ()
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
               (cons "MAP" (lambda ()
                             (set-dispatch-macro-character #\# #\[ 'map-reader-macro)
                             (set-macro-character #\] (lambda (stream char)
                                                        (declare (ignore stream char))
                                                        (error "No matching [ for ]")))))
               (cons "HASH-SET" (lambda ()
                                  (set-dispatch-macro-character #\# #\{
                                                                'hash-set-reader-macro)
                                  (set-macro-character #\}
                                                       (lambda (stream char)
                                                         (declare (ignore stream char))
                                                         (error "No matching { for }"))))))))
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (mapcar (lambda (reader-macro-identifier)
              (funcall (cdr (assoc reader-macro-identifier
                                   reader-macro-activation-functions
                                   :test #'string=))))
            reader-macro-identifier-strings)))

(defmacro enable-reader-syntax (&rest reader-macro-identifiers)  
  "READER-MACRO-IDENTIFIERS are any of the following symbols:
  LAMBDA, GET-VAL, HASH-TABLE, MAP, HASH-SET"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-reader-syntax ,@reader-macro-identifiers)))

(defun %disable-reader-syntax ()
  (setq *readtable* (if *previous-readtables*
                        (pop *previous-readtables*)
                        (copy-readtable nil))))

(defmacro disable-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-reader-syntax)))

(named-readtables:defreadtable reader
  (:merge :standard)
  (:macro-char #\GREEK_SMALL_LETTER_LAMDA 'lambda-reader-macro)
  (:macro-char #\[ 'get-val-reader-macro)
  (:macro-char #\] (lambda (stream char)
                     (declare (ignore stream char))
                     (error "No matching [ for ]")))
  (:macro-char #\{ 'hash-table-reader-macro)
  (:macro-char #\} (lambda (stream char)
                     (declare (ignore stream char))
                     (error "No matching { for }")))
  (:dispatch-macro-char #\# #\[ 'map-reader-macro)
  (:dispatch-macro-char #\# #\] (lambda (stream char)
                                  (declare (ignore stream char))
                                  (error "No matching [ for ]")))
  (:dispatch-macro-char #\# #\{ 'hash-set-reader-macro)
  (:dispatch-macro-char #\# #\} (lambda (stream char)
                                  (declare (ignore stream char))
                                  (error "No matching { for }"))))

(defun read-stream-until (stream until-char)
  (iter (for next-char = (peek-char t stream nil))
        (unless next-char (next-iteration))
        (until (char= until-char next-char))
        (collect (read stream nil))
        (finally (read-char stream nil))))

;;; Consider the case when the package is not "used" and the user used "-" and "---"
;;; in the function body. Then, we need to include "--" from this package, and
;;; and other symbols must either be kept in their original packages, or
;;; all the symbols must be replaced by the symbols in this package consistently.
;;; We choose the first method.
(defun construct-parameter-list (body may-be-body)
  (let* ((arg-symbols (remove-if-not
                       (lambda (elt)
                         (and (symbolp elt)
                              (member (symbol-name elt) '("-" "--" "---")
                                      :test 'string=)))
                       (alexandria:flatten body)))
         (n (if (typep may-be-body '(integer 0 3))
                may-be-body
                (cond ((member "---" arg-symbols :test 'string=) 3)
                      ((member "--" arg-symbols :test 'string=) 2)
                      ((member "-" arg-symbols :test 'string=) 1)
                      (t 0)))))
    (iter (for i below n)
          (for arg-symbol = (car arg-symbols))
          (for our-arg-name
               initially "-"
               then (concatenate 'string our-arg-name "-"))
          (if (and arg-symbol
                   (string= (symbol-name arg-symbol)
                            our-arg-name))
              (progn (collect arg-symbol)
                     (setq arg-symbol (cdr arg-symbols)))
              (progn (collect (intern our-arg-name)))))))

(defun lambda-reader-macro (stream char)
  (declare (ignore char))
  (let* ((may-be-body (read stream))
         (body (if (typep may-be-body '(integer 0 3))
                   (read stream)
                   may-be-body))
         (parameter-list (construct-parameter-list body may-be-body)))
    `(lambda (&optional ,@parameter-list &rest args)
       (declare (ignorable ,@parameter-list args))
       ,body)))

(defun map-reader-macro (stream char n)
  (declare (ignore char n))
  (let ((args (read-stream-until stream #\])))
    `(generic-cl:map ,@args)))

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
  (assert (= 1 (length key/s)))
  (let ((key (car key/s)))
    (cond ((trivial-types:association-list-p object)
           (let ((pair (assoc key object :test 'equal)))
             (if pair (cdr pair) nil)))
          ((and (trivial-types:property-list-p object)
                (not (integerp key)))
           (getf object key))
          (t (nth key object)))))

(defmethod (setf get-val) (new-value (object list) &rest key/s)
  (assert (= 1 (length key/s)))
  (setf (nth (car key/s) object) new-value))

(defmethod get-val ((object array) &rest key/s)
  (apply #'numcl:aref object key/s))

(defmethod (setf get-val) (new-value (object array) &rest key/s)
  (setf (apply #'numcl:aref object key/s) new-value))

(defmethod get-val ((object simple-array) &rest key/s)
  (apply #'aref object key/s))

(defmethod (setf get-val) (new-value (object simple-array) &rest key/s)
  (setf (apply #'aref object key/s) new-value))

(defun get-val-reader-macro (stream char)
  (declare (ignore char))
  `(get-val ,@(read-stream-until stream #\])))

(defun hash-table-reader-macro (stream char)
  (declare (ignore char))
  (let* ((inputs (read-stream-until stream #\}))
         (test (case (first inputs)
                 (:eq 'eq)
                 (:eql 'eql)
                 (:equalp 'equalp)
                 (t 'equal)))
         (first-is-test (member (first inputs) '(:eq :eql :equalp :equal)))
         (inputs (if first-is-test
                     (cdr inputs)
                     inputs)))
    (unless (evenp (length inputs))
      (error (format nil
                     "Constructing a hash table requires even number of elements: ~d"
                     inputs)))
    `(alexandria:plist-hash-table (list ,@inputs) :test ',test)))

(defun hash-set-reader-macro (stream char n)
  (declare (ignore char n))
  `(list-to-hs (list ,@(iter (until (char= (peek-char t stream nil)
                                           #\}))
                             (collect (read stream))
                             (finally (read-char stream nil))))))
