(defpackage :reader
  (:use :named-readtables :cl :iterate :arrow-macros :hash-set)
  (:export
   :-
   :--
   :---
   :cl-reader
   :get-val))

(in-package :reader)


(named-readtables:defreadtable cl-reader
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
        (while next-char)
        (until (char= until-char next-char))
        (collect (read stream nil))
        (finally (read-char stream nil))))

(defun lambda-reader-macro (stream char) 
  (declare (ignore char))
  `(lambda (&optional - -- ---)
     (declare (ignorable - -- ---))
     ,(read stream)))

(defun mapcar-reader-macro (stream char n)
  (declare (ignore char n))
  `(mapcar ,@(iter (for next-char = (peek-char t stream nil))
                   (while next-char)
                   (until (char= #\] next-char))
                   (collect (read stream))
                   (finally (read-char stream nil)))))

(defmacro defmethods (fun-name lambda-list &rest methods)
  `(defgeneric ,fun-name ,lambda-list
     ,@(loop for args = (first methods)
          for body = (second methods)
          do (setq methods (cddr methods))
          while methods
          collect `(:method ,args ,body))))

(defmethods get-val (object key)
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
