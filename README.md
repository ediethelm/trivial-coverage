# Trivial Coverage Manual

###### \[in package TRIVIAL-COVERAGE\]
[![pipeline status](https://gitlab.com/ediethelm/trivial-coverage/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-coverage/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-coverage.svg)](http://quickdocs.org/trivial-coverage/)

## Description

A simple Common Lisp library to print out the code coverage collected by sb-cover. As such it only supports SBCL.

## Installing trivial-coverage

This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-coverage)
```

But if you want access to the latest updates, install it by cloning the Git repository with

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-coverage.git
```

and then loading it as usual via [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") as above.

## Working Example

Assuming there is a system being tested and named :system-under-test, which has three files that should not be included in the coverage calculation. The following expression loads the system, executes the tests and reports the total coverage:  

`lisp
(trivial-coverage:test-and-print-coverage :system-under-test :exclude '("test-system-under-test.lisp" 
                                                                        "test-data-generator.lisp"
                                                                        "documentation.lisp"))
`

To access the coverage value programatically the following is possible:  

```lisp
(declaim (optimize sb-cover:store-coverage-data))
((asdf:oos 'asdf:load-op :system-under-test)
(let ((report-path (merge-pathnames "coverage-report/" (asdf:system-source-directory :system-under-test))))
	   (sb-cover:reset-coverage)
	   (asdf:oos 'asdf:test-op :system-under-test)
	   (sb-cover:report report-path)
	   (get-coverage report-path :exclude exclude))
```


## Exported Symbols

- [function] TEST-AND-PRINT-COVERAGE SYSTEM &KEY EXCLUDE KEEP-REPORT (STREAM T)

    Loads and tests the given *system* collecting coverage information. Also prints a line containing the coverage to *stream*.  
    *system* - name of the system to be tested  
    *exclude* - files to be excluded from the calculation  
    *keep-report* - if NIL, the generated HTML coverage files are removed  
    *stream* - the stream to which the coverage result shall be written

- [function] GET-COVERAGE REPORT-PATH &KEY EXCLUDE

    Calculates the total code coverage reported by sb-cover (via the HTML report) and returns this value.  
    *report-path* - the path to the directory containing the HTML coverage report  
    *exclude* - files to be excluded from the calculation

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-coverage/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-coverage/blob/master/CONTRIBUTING.md "Contributing") document for more information.

