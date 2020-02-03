(in-package :cl-user)
(defpackage reader-test-framework
  (:documentation
   "A prove-inspired minimal testing framework based around is- forms and 
grouping tests on the basis of packages.")
  (:use :cl :iterate)
  (:import-from :alexandria
                :with-gensyms
                :once-only
                :ensure-gethash
                :hash-table-values)
  (:export :*default-test-function*
           :*debug-on-error*

           :is
           :is-type

           :define-test
           :run-test
           :run-test-package
           :run-test-all
           :remove-test
           :remove-test-all))

(in-package :reader-test-framework)


;;; The following gensym- entities are required by is-expand.

(defvar *gensym-prefix* "$")
(defvar *gensym-alist*)

(defun gensymp (val)
  (and (symbolp val)
       (string= (subseq (symbol-name val) 0 (length *gensym-prefix*)) *gensym-prefix*)))

(defgeneric gensym-tree-equal (x y)
  (:method (x y)
    (if (and (gensymp y) (symbolp x))
        (if (assoc y *gensym-alist*)
            (eq x (cdr (assoc y *gensym-alist*)))
            (unless (rassoc x *gensym-alist*)
              (setf *gensym-alist* `((,y . ,x) ,@*gensym-alist*))
              t))
        (equal x y)))
  (:method ((x cons) (y cons))
    (loop for a in x for b in y
       always (gensym-tree-equal a b))))

;;; Next three forms define various is- forms

(defvar *whole-form*)
(setf (documentation '*whole-form* 'variable)
      "Intended for internally using IS to generate other IS- forms, avoiding additional parameters and the multi-level macros.")

(defmacro is (&whole whole got expected &key (test 'cl:equal test-supplied-p))
  "WHOLE-EXPLICIT is intended for internal usage."
  (let ((return-value (gensym)))
    `(let (,return-value
           (*whole-form* (if (boundp '*whole-form*)
                             *whole-form*
                             ',whole)))
       (if (funcall ,(if test-supplied-p test ''cl:equal)
                    (setq ,return-value
                          (handler-case ,got
                            (error (e)
                              (incf *num-errored*)
                              (format nil "~D" e))))
                    ,expected)
           (incf *num-passed*)
           (progn
             (format t "    ~D did not pass. Got ~D.~%"
                     *whole-form*
                     ,return-value)
             (incf *num-failed*))))))

(defmacro define-is-clause (name lambda-list is-form)
  (let ((whole (gensym)))
    `(defmacro ,name (&whole ,whole ,@lambda-list)
       `(let ((*whole-form* (if (boundp '*whole-form*)
                                *whole-form*
                                ',,whole)))
          ,,is-form))))

(define-is-clause is-true (got) `(is ,got t))
(define-is-clause is-type (got type) `(is-true (typep ,got ,type)))

;;; The remaining forms help define and run the tests at the global level.

(defvar *package-tests* (make-hash-table))
(defvar *num-errored*)
(defvar *num-passed*)
(defvar *num-failed*)

(defmacro define-test (name &body test-forms)
  "Defines a test comprising of TEST-FORMS with NAME under *PACKAGE*."
  (with-gensyms (tests)
    `(let ((,tests (ensure-gethash *package* *package-tests* (make-hash-table))))
       (setf (gethash ',name ,tests)
             (lambda ()
               ;; this defines all the work (including background work) that
               ;; should happen when a test is run
               (format t "~A~%" ',name)
               ,@test-forms
               (let* ((base-string
                       (format nil "  ~D passed ~D failed ~D errored."
                               *num-passed* *num-failed* *num-errored*))
                      (rem-length (- 80 (length base-string)))
                      (num-hyphens (- rem-length 14)))
                 (write-string base-string)
                 (unless (= 0 *num-failed* *num-errored*)
                   (loop initially (write-char #\space)
                        (write-char #\<)
                      for i below (max num-hyphens 0)
                      do (write-char #\-)
                      finally (write-char #\space)
                        (write-string "not passing")))
                 (terpri)))))))

(defun run-test (name)
  (let ((tests (gethash *package* *package-tests*)))
    (when tests (funcall (gethash name tests)))))

(defun run-test-package (package-designator)
  "Run tests under package designated by PACKAGE-DESIGNATOR."
  (let ((functions (hash-table-values (gethash (find-package package-designator)
                                               *package-tests*)))
        (total-passed 0)
        (total-errored 0)
        (total-failed 0))
    (declare (special *num-passed* *num-errored* *num-failed*))
    (dolist (function functions)
      (let ((*num-passed* 0)
            (*num-errored* 0)
            (*num-failed* 0))
        (funcall function)
        (incf total-passed *num-passed*)
        (incf total-errored *num-errored*)
        (incf total-failed *num-failed*)))
    (if (= 0 total-errored total-failed)
        (format t "~%ALL TESTS PASSED!~%")
        (format t "~%ALL TESTS DID NOT PASS!~%~D PASSED ~D FAILED ~D ERRORED.~%"
                total-passed total-failed total-errored))))

(defun remove-test (name)
  "Remove test with NAME under *PACKAGE*."
  (let ((tests (gethash *package* *package-tests*)))
    (when tests (remhash name tests))))


