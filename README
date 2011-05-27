pycoverage.el
=============

An emacs mode for reporting on coverage stats for python 2.x

Dependencies
============

Coverage reporter for Python:

  * coverage.py (currently we only support this)
  * figleaf (support pending)
  * nose (there is limited supported for testing a method/function
    using nose)

Installation
============

Put something like this in your .emacs

(load-file "/home/matt/work/emacs/pycoverage/pycov2.el")
(require 'linum)
(require 'pycov2)
(add-hook 'python-mode-hook
(function (lambda ()
(pycov2-mode)
(linum-mode))))


Install cov2emacs using setuptools or virtualenv or distutils

There should be ``.coverage`` file in the directory of the module you
want coverage reporting on (or the parents of that directory).  If no
coverage file is found, you may specify one useing pycov2-rerun.  Note
that if your file has been modified later than the .coverage file, it
will be considered as stale and ignore it.



Running
=======

M-x pycov2-mode


Ideal Usage
===========

If there is a .coverage file in the directory (or a parent) of the
source file try to use it for coverage information.  Red highlights
mean that lines were missed (Coverage percent for file is in mode
line).  Orange means .coverage file is missing or OLD.  Purple means
the cov2emacs file is not found, you need to put it in PATH or set
pycov2-cov2emacs-cmd.

pycov2-rerun will re-run the coverage stats for .coverage file located
elsewhere.

pycov2-test-function (not implemented) will try to find tests for that
function and run them.

Todo
====

  * Make invalidate data when you save file!

  * Make it work! Semi-DONE
  * Use flymake mode instead of/in combination with compile mode? - Initial Flymake DONE
  * Use missing line numbers instead of covered lines - DONE for coverage.py
  * Make pycoverage-load-report look for a ``.coverage`` file
    recursively up the parents of the file - DONE
  * Make ``cov2emacs`` accept location of ``.coverage`` file - DONE
  * Make report use normal coverage.py text output, since it's a
    little friendlier on the eyes (instead of reporting for every
    group of lines in a file)
  * Put status in modeline - DONE
    * Number - current coverage %
    * OLD - dirty
    * ? - no .coverage file available
      Note that if coverage file has not been specified (pycov2-rerun)
      it will skip over old .coverage files it finds  (see .cov2emacs.log)
    * ERR - Error (see *messages*)
  * Figleaf support
  * Nose integration?

Thanks
======

  * rcov - for code to start from
