;;;; Copyright (c) Eric Diethelm 2019 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-coverage)

(defsection @trivial-coverage-manual (:title "Trivial Coverage Manual")
  "[![pipeline status](https://gitlab.com/ediethelm/trivial-coverage/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-coverage/commits/master)
  [![Quicklisp](http://quickdocs.org/badge/trivial-coverage.svg)](http://quickdocs.org/trivial-coverage/)"
  (@trivial-coverage-description section)
  (@trivial-coverage-installing section)
  (@trivial-coverage-example section)
  (@trivial-coverage-exported section)
  (@trivial-coverage-license section)
  (@trivial-coverage-contributing section))


(defsection @trivial-coverage-description (:title "Description")
  "A simple Common Lisp library to print out the code coverage collected by sb-cover. As such it only supports SBCL.")

(defsection @trivial-coverage-installing (:title "Installing trivial-coverage")
    "This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-coverage)
```

But if you want access to the latest updates, install it by cloning the Git repository with

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-coverage.git
```

and then loading it as usual via [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") as above.
")

(defsection @trivial-coverage-example (:title "Working Example")
  "Assuming there is a system being tested and named :system-under-test, which has three files that should not be included in the coverage calculation. The following expression loads the system, executes the tests and reports the total coverage:  

  ```lisp
(trivial-coverage:test-and-print-coverage :system-under-test :exclude '(\"test-system-under-test.lisp\" 
                                                                        \"test-data-generator.lisp\"
                                                                        \"documentation.lisp\"))
```

To access the coverage value programatically the following is possible:  

```lisp
(declaim (optimize sb-cover:store-coverage-data))
(let ((report-path (merge-pathnames \"coverage-report/\" (asdf:system-source-directory :system-under-test))))
	   (sb-cover:reset-coverage)
	   (asdf:oos 'asdf:test-op :system-under-test)
	   (sb-cover:report report-path)
	   (get-coverage report-path :exclude exclude))
```

")

(defsection @trivial-coverage-exported (:title "Exported Symbols")
  (test-and-print-coverage function)
  (get-coverage function))

(defsection @trivial-coverage-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-coverage/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-coverage-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-coverage/blob/master/CONTRIBUTING.md 'Contributing') document for more information.")
