;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-coverage
  :name "trivial-coverage"
  :description "A simple Common Lisp library to print out the code coverage collected by sb-cover. As such it only supports SBCL."
  :version "0.0.1"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:lquery
               :mgl-pax)
  :components ((:file "package")
	       (:file "trivial-coverage")
	       (:file "documentation")))

