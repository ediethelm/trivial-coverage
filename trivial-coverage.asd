;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-coverage
  :name "trivial-coverage"
  :description ""
  :version "0.0.1"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:mgl-pax)
  :components ((:file "package")
	       (:file "trivial-coverage")
	       (:file "documentation")))

