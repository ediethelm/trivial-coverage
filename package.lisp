(uiop:define-package #:trivial-coverage
  (:documentation "A simple Common Lisp library to print out the code coverage collected by sb-cover. As such it only supports SBCL.")
  (:use #:common-lisp)
  (:export #:test-and-print-coverage
           #:get-coverage))

