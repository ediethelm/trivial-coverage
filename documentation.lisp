;;;; Copyright (c) Eric Diethelm 2019 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-coverage)

(defsection @trivial-coverage-manual (:title "Trivial GitLab API Manual")
  "[![pipeline status](https://gitlab.com/ediethelm/trivial-coverage/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-coverage/commits/master)
  [![Quicklisp](http://quickdocs.org/badge/trivial-coverage.svg)](http://quickdocs.org/trivial-coverage/)"
  (@trivial-coverage-description section)
  (@trivial-coverage-installing section)
  (@trivial-coverage-example section)
  (@trivial-coverage-exported section)
  (@trivial-coverage-license section)
  (@trivial-coverage-contributing section))


(defsection @trivial-coverage-description (:title "Description")
  "")

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
  "
  ```lisp
```
")

(defsection @trivial-coverage-exported (:title "Exported Symbols")
)

(defsection @trivial-coverage-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-coverage/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-coverage-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-coverage/blob/master/CONTRIBUTING.md 'Contributing') document for more information.")
