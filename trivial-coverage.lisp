;;;; Copyright (c) Eric Diethelm 2019 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

#+sbcl (eval-when (:compile-toplevel :load-toplevel)
	   (require :sb-cover))

(in-package #:trivial-coverage)

(defun get-coverage-for-file (html)
  (let ((doc (lquery:$ (lquery:initialize html))))
    (lquery:$ (inline (lquery:$ doc ".summary")) "tr"
	      #'(lambda (elms) (let ((vals (lquery:$ (inline (aref elms 1)) "td")))
				 (list (string-right-trim '(#\Newline #\Space)
							  (subseq (aref (lquery:$ doc "h3" (text)) 0)
								  (length "Coverage report: ")))
				       (parse-integer (aref (lquery:$ (inline (aref vals 1)) (text)) 0))
				       (parse-integer (aref (lquery:$ (inline (aref vals 2)) (text)) 0))))))))

(defun get-coverage (report-path &key exclude)
  "Calculates the total code coverage reported by sb-cover (via the HTML report) and returns this value.  
*report-path* - the path to the directory containing the HTML coverage report  
*exclude* - files to be excluded from the calculation"
  (let ((coverage (remove-if #'(lambda (s) (member (file-namestring (car s))
						   exclude
						   :test #'string=))
			     (mapcar #'(lambda (html) (get-coverage-for-file html))
				     (remove "cover-index.html"
					     (uiop:directory-files report-path)
					     :key #'file-namestring
					     :test #'string=)))))

    (let ((divisor (reduce #'+ coverage :key #'third)))
      (if (eq 0 divisor)
	  0
	  (/ (reduce #'+ coverage :key #'second) divisor)))))

(defun pprint-coverage (coverage &key (stream t))
  (format stream "~%Test Coverage: ~1$%~%" (* 100 coverage)))

(defun test-and-print-coverage (system &key exclude keep-report (stream t))
  "Loads and tests the given *system* collecting coverage information. Also prints a line containing the coverage to *stream*.  
*system* - name of the system to be tested  
*exclude* - files to be excluded from the calculation  
*keep-report* - if NIL, the generated HTML coverage files are removed  
*stream* - the stream to which the coverage result shall be written"
  #+sbcl (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op system :force t)
  #+sbcl (let ((report-path (merge-pathnames "coverage-report/" (asdf:system-source-directory system))))
	   (sb-cover:reset-coverage)
	   (asdf:oos 'asdf:test-op system)
	   (sb-cover:report report-path)
	   (pprint-coverage (get-coverage report-path :exclude exclude) :stream stream)
	   (unless keep-report
	     (uiop:delete-directory-tree report-path :validate t)))
  #-sbcl (asdf:oos 'asdf:test-op system)
  #-sbcl (warn "trivial-coverage only supports SBCL. Tests were executed, but no coverage information was collected.")
  #+sbcl (declaim (optimize (sb-cover:store-coverage-data 0))))
  
