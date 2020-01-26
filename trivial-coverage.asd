;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-coverage
  :name "trivial-coverage"
  :description "A simple Common Lisp library to print out the code coverage collected. Supports SBCL and CCL."
  :version "0.0.4"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (#+sbcl :lquery
	       #+ccl :cl-csv)
  :components ((:file "package")
	       (:file "trivial-coverage")))

