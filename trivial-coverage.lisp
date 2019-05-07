
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
  (let ((coverage (remove-if #'(lambda (s) (member (file-namestring (car s))
						   exclude
						   :test #'string=))
			     (mapcar #'(lambda (html) (get-coverage-for-file html))
				     (remove "cover-index.html"
					     (uiop:directory-files report-path)
					     :key #'file-namestring
					     :test #'string=)))))
    (/ (reduce #'+ coverage :key #'second)
       (reduce #'+ coverage :key #'third))))

(defun pprint-coverage (coverage)
  (format t "~%Test Coverage: ~1$%~%" (* 100 coverage)))

(defun test-and-print-coverage (system &key exclude)
  (declaim (optimize sb-cover:store-coverage-data))
  (asdf:oos 'asdf:load-op system)
  (let ((report-path (merge-pathnames "coverage-report/" (asdf:system-source-directory system))))
    (sb-cover:reset-coverage)
    (asdf:oos 'asdf:test-op system)
    (sb-cover:report report-path)
    (pprint-coverage (get-coverage report-path :exclude exclude))
    (uiop:delete-directory-tree report-path :validate t))
  (declaim (optimize (sb-cover:store-coverage-data 0))))
  
