;;;; Copyright (c) Eric Diethelm 2019 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(eval-when (:compile-toplevel :load-toplevel)
  #+sbcl (require :sb-cover)
  #-(or ccl sbcl) (warn "trivial-coverage only supports SBCL and CCL. Tests will executed, but no coverage information will collected."))

(in-package #:trivial-coverage)

(defun enable-coverage ()
  #+sbcl (declaim (optimize sb-cover:store-coverage-data))
  #+ccl (setq ccl:*compile-code-coverage* t))

(defun disable-coverage ()
  #+sbcl (declaim (optimize sb-cover:store-coverage-data 0))
  #+ccl (setq ccl:*compile-code-coverage* nil))

(defun clear-coverage ()
  #+sbcl (sb-cover:reset-coverage)
  #+ccl (ccl:reset-coverage))

(defun create-coverage-report (path)
  #+sbcl (sb-cover:report path)
  #+ccl (ccl:report-coverage (merge-pathnames "cover-index.html" path) :html nil :statistics t))

#+ccl
(defun get-coverage (report-path &key exclude)
  ;; https://ccl.clozure.com/docs/ccl.html

  (let* ((data (cl-csv:read-csv (merge-pathnames "statistics.csv" report-path) :separator #\,))
	 (source-pos (position "Source file" (car data) :test #'string=))
	 (total-pos (position "Expressions Total"  (car data) :test #'string=))
	 (result-pos (position "Expressions Entered" (car data) :test #'string=)))
    (iterate:iterate
      (iterate:for elm in (cdr data))
      (unless (member (nth source-pos elm) exclude :test #'(lambda (x y)
							     (search x y :test #'string=)))
	(iterate:collect (parse-integer (nth result-pos elm)) into total)
	(iterate:collect (parse-integer (nth total-pos elm)) into entered))
      (iterate:finally (let ((sum-expressions (reduce #'+ total))
			     (sum-entered (reduce #'+ entered)))
			 (return (if (> sum-expressions 0)
				     (/ sum-entered sum-expressions)
				     0)))))))

#+sbcl
(defun get-coverage-for-file (html)
  (let ((doc (lquery:$ (lquery:initialize html))))
    (lquery:$ (inline (lquery:$ doc ".summary")) "tr"
	      #'(lambda (elms) (let ((vals (lquery:$ (inline (aref elms 1)) "td")))
				 (list (string-right-trim '(#\Newline #\Space)
							  (subseq (aref (lquery:$ doc "h3" (text)) 0)
								  (length "Coverage report: ")))
				       (parse-integer (aref (lquery:$ (inline (aref vals 1)) (text)) 0))
				       (parse-integer (aref (lquery:$ (inline (aref vals 2)) (text)) 0))))))))

#+sbcl
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
  (enable-coverage)
  (asdf:oos 'asdf:load-op system :force t)
  (let ((report-path (merge-pathnames "coverage-report/" (asdf:system-source-directory system))))
    (clear-coverage)
    (asdf:oos 'asdf:test-op system)
    (create-coverage-report report-path)

    #+(or ccl sbcl) (pprint-coverage (get-coverage report-path :exclude exclude) :stream stream)
    #-(or ccl sbcl) (warn "trivial-coverage only supports SBCL and CCL. Tests were executed, but no coverage information was collected.")

    (unless keep-report
      (uiop:delete-directory-tree report-path :validate t)))

  (disable-coverage))
  
