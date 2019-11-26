(defpackage :reader
  (:use :named-readtables :cl :iterate :arrow-macros :hash-set)
  (:export
   :-
   :--
   :---
   :args
   :reader
   :get-val))

(in-package :reader)


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

(defun guess-num-args (body)
  (let* ((symbols (alexandria:flatten body))
         (arg-symbols (intersection symbols '(- -- ---))))
    (cond ((member '--- arg-symbols) 3)
          ((member '-- arg-symbols) 2)
          ((member '- arg-symbols) 1)
          (t 0))))

(defun lambda-reader-macro (stream char) 
  (declare (ignore char))
  (let* ((may-be-body (read stream))
         (num-args-p (if (typep may-be-body '(integer 0 3))
                        may-be-body
                        nil))
         (num-args (if (typep may-be-body '(integer 0 3))
                       may-be-body
                       (guess-num-args may-be-body)))
         (body (if num-args-p (read stream) may-be-body)))
    `(lambda ,@(ecase num-args
                 (0 `((&optional &rest args)
                      (declare (ignorable args))))
                 (1 `((&optional - &rest args)
                      (declare (ignorable - args))))
                 (2 `((&optional - -- &rest args)
                      (declare (ignorable - -- args))))
                 (3 `((&optional - -- --- &rest args)
                      (declare (ignorable - -- --- args)))))
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
