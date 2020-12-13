(cl:defpackage :reader+swank
  (:use :cl :reader)
  (:export
   :enable-package-local-reader-syntax
   :disable-package-local-reader-syntax))

(in-package :reader+swank)

(defmacro enable-package-local-reader-syntax (&rest reader-macro-identifiers)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get-val swank:*readtable-alist* (package-name *package*) :test #'string=)
           (progn
             (setq *readtable* (copy-readtable))
             (reader::%enable-reader-syntax *readtable* ,@reader-macro-identifiers)
             *readtable*))))

(defmacro disable-package-local-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf swank:*readtable-alist*
           (remove (package-name *package*)
                   swank:*readtable-alist*
                   :key #'first
                   :test #'string=))))

(setf (documentation 'enable-package-local-reader-syntax 'function)
      (concatenate 'string
                   reader::+reader-macro-doc+
                   (string #\newline)
                   "Package local refers to CL:*PACKAGE*."))

