#!/usr/bin/env python
# Copyright (c) 2009 Matt Harrison

import sys
import optparse
import os
import cPickle as pickle

import meta

COVERED = 'Error'#'Covered'
IGNORED = 'Ignored'
MISSED = 'Missed'



class Coverage2Emacs(object):
    """
    Convert coverage.py data to something emacs likes
    """
    def __init__(self, cov_file):
        if os.path.basename(cov_file) != '.coverage':
            raise Exception('wrong filename %s' % cov_file)
        self.cov_file = cov_file

    def to_emacs_new(self, fout=None, filenames=None):
        from coverage import control
        cov = coverage(data_file=self.cov_file)
        pass
        
        
    def to_emacs(self, fout=None, filenames=None):
        """
        spit out something like this that emacs understands

        filename:linenumber:message

        Message can be Covered|Ignored|Missed
        """
        fout = fout or sys.stdout
        for filename, line, status in self.filter_old_files(self.data_tuples(filenames)):
            fout.write('%s:%s:%s\n' %(filename, line, status))
            
    def data_tuples(self, filenames=None):
        filenames = filenames or []
        fin = open(self.cov_file, 'r')
        data = pickle.load(fin)
        files = data['lines'].keys()
        files.sort()
        for filename in files:
            if filenames and filename not in filenames:
                continue
            for line in combine_linenums(data['lines'][filename]):
                yield filename, line, COVERED

    
    def filter_old_files(self, data_iter):
        cov_date = os.stat(self.cov_file).st_mtime
        file_date = None
        prev_file = None
        for filename, line, status in data_iter:
            if prev_file is None or prev_file != filename:
                file_date = os.stat(filename).st_mtime

            if file_date > cov_date:
                # assume that file has been tweeked and data is wrong
                continue

            yield filename, line, status
            prev_file = filename
            
def combine_linenums(linenums):
    """
    >>> list(combine_linenums([1,2,3]))
    ['1-3']
    >>> list(combine_linenums([1,3]))
    ['1', '3']
    >>> list(combine_linenums([1,3,5,6,7]))
    ['1', '3', '5-7']
    >>> list(combine_linenums([1,2,4,5,6]))
    ['1-2', '4-6']
    """
    prev_start = None
    prev_num = None
    for num in linenums:
        if prev_start is None:
            prev_start = num
        elif prev_num + 1 != num:
            if prev_start == prev_num:
                yield '%d' % prev_num
            else:
                yield '%d-%d' %(prev_start, prev_num)
            prev_start = num
        prev_num = num

    if prev_num and prev_start:
        if prev_start == prev_num:
            yield '%d' % prev_num
        else:    
            yield '%d-%d' %(prev_start, num)
    elif prev_num:
        yield '%d' % prev_num
            
def _test():
    import doctest
    doctest.testmod()
                              

def main(prog_args):
    parser = optparse.OptionParser(version=meta.__version__)
    opt, args = parser.parse_args(prog_args)
    # remove hardcode
    c2e = Coverage2Emacs('~/.coverage')
    c2e.to_emacs()
    
if __name__ == '__main__':
    sys.exit(main(sys.argv))

