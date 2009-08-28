# Copyright (c) 2009 Matt Harrison
from distutils.core import setup
#from setuptools import setup

from cov2emacslib import meta

setup(name='cov2emacs',
      version=meta.__version__,
      author=meta.__author__,
      description='FILL IN',
      scripts=['bin/cov2emacs'],
      package_dir={'cov2emacslib':'cov2emacslib'},
      packages=['cov2emacslib'],
)
