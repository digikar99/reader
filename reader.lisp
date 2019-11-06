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
  (:dispatch-macro-char #\# #\[ 'mapcar-reader-macro)
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
         (num-args-p (if (typep may-be-body '(integer 1))
                        may-be-body
                        nil))
         (num-args (if (typep may-be-body '(integer 1))
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

(defun mapcar-reader-macro (stream char n)
  (declare (ignore char n))
  `(mapcar ,@(iter (for next-char = (peek-char t stream nil))
                   (while next-char)
                   (until (char= #\] next-char))
                   (collect (read stream))
                   (finally (read-char stream nil)))))

(defmacro defmethods-with-setf (fun-name lambda-list &rest methods)
  `(progn
     ,@(let ((new-value (gensym)))
         `(,(let ((methods methods))
              `(defgeneric ,fun-name ,lambda-list
                 ,@(loop for args = (first methods)
                      for body = (second methods)
                      do (setq methods (cddr methods))
                      while methods
                      collect `(:method ,args ,body))))
            ,@(let ((methods methods))
                (loop for args = (first methods)
                   for body = (second methods)
                   while methods
                   collect `(defmethod (setf ,fun-name) (,new-value ,@args)
                              (setf ,body ,new-value))
                   do (setq methods (cddr methods))))))))

(defmethods-with-setf get-val (object key)
  ((object vector) key) (aref object key)
  ((object array) key) (apply #'aref object key)
  ((object hash-table) key) (gethash key object)
  ((object list) key) (nth key object)
  ((object sequence) key) (elt object key)
  ((object structure-object) key) (slot-value object key)
  ((object standard-object) key) (slot-value object key))

(defun get-val-reader-macro (stream char)
  (declare (ignore char))
  `(-> (get-val ,(read stream) ,(read stream))
     ,@(iter (until (char= (peek-char t stream nil)
                           #\]))
             (collect `(get-val ,(read stream)))
             (finally (read-char stream nil)))))

(defun hash-table-reader-macro (stream char)
  (declare (ignore char))
  (let ((inputs (read-stream-until stream #\})))
    (unless (evenp (length inputs))
      (error (format nil
                     "Constructing a hash table requires even number of elements: ~d"
                     inputs)))
    `(alexandria:plist-hash-table (list ,@inputs) :test 'equal)))

(defun hash-set-reader-macro (stream char n)
  (declare (ignore char n))
  `(list-to-hs (list ,@(iter (until (char= (peek-char t stream nil)
                                           #\}))
                             (collect (read stream))
                             (finally (read-char stream nil))))))
