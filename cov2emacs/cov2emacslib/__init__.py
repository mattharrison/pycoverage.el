#!/usr/bin/env python
# Copyright (c) 2009 Matt Harrison

import sys
import optparse
import os

from coverage.report import Reporter
from coverage.control import coverage as cv

import meta

COVERED = 'Error'#'Covered'
IGNORED = 'Ignored'
MISSED = 'Missed'

class BasicReporter(Reporter):
    """
    Hacked subclass of coverage.py Reporter that instead of actually
    doing anything just yields the data.

    Since the .coverage file only contains the line's covered we need
    to use Coverage.py's logic to determine the 'missing' lines.
    """
    def __init__(self, report_file, ignore_errors=False):
        coverage = cv(report_file)
        coverage.use_cache(True)
        coverage.load()
        super(BasicReporter, self).__init__(coverage, ignore_errors)
        
    def report(self, morfs=None, directory=None, omit_prefixes=None):
        for result in self.report_files(morfs, directory, omit_prefixes):
            yield result

    def report_files(self, morfs, directory=None,
                     omit_prefixes=None):
        """Run a reporting function on a number of morfs.

        No callback function, just yield the cu, statements, excluded and missing
        """
        self.find_code_units(morfs, omit_prefixes)

        self.directory = directory
        if self.directory and not os.path.exists(self.directory):
            os.makedirs(self.directory)

        for cu in self.code_units:
            try:
                # don't filter relative!!!
                # if not cu.relative:
                #     continue
                statements, excluded, missing, _ = self.coverage._analyze(cu)
                yield (cu, statements, excluded, missing)
            except KeyboardInterrupt:
                raise
            except:
                if not self.ignore_errors:
                    raise


class Coverage2Emacs(object):
    """
    Convert coverage.py data to something emacs likes
    """
    def __init__(self, cov_file):
        if os.path.basename(cov_file) != '.coverage':
            raise Exception('wrong filename %s' % cov_file)
        self.cov_file = cov_file

    def to_emacs_flymake_mode(self, filename, fout=None):
        """
        flymake mode output looks like this:
        filename:lineno: Warning|Error (function):msg
        """
        fout = fout or sys.stdout
        reporter = BasicReporter(self.cov_file)
        for cu, statements, excluded, missing in reporter.report():
            if cu.filename != filename:
                # probably could filter earlier to speed things up
                continue
            for line in missing:
                fout.write('%s:%s: Error Line not covered by test\n' %(cu.filename, line))

            
    def to_emacs_compile_mode(self, fout=None, filenames=None):
        """
        spit out something like this that emacs understands

        filename:linenumber:message

        Message can be Covered|Ignored|Missed
        """
        filenames = filenames or []
        fout = fout or sys.stdout
        reporter = BasicReporter(self.cov_file)
        # covert the report output to a more useful generator
        data_iter = ((cu.filename, missing, 'MISSING') for cu, statements, excluded, missing in reporter.report())
        filtered_names = self.filter_old_files(data_iter)
        for filename, line, status in filtered_names:
            if filenames and filename not in filenames:
                continue
            for line_chunk in combine_linenums(line):
                fout.write('%s:%s:%s\n' %(filename, line_chunk, status))
        
    
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


def find_coverage_file(start_file, file_to_find='.coverage'):
    """
    starting from start_file, look it its directory and go up the
    parents until you find a matching file
    """
    start_path = os.path.abspath(start_file)
    start_dir = os.path.dirname(start_path)
    done = False
    while not done:
        possible = os.path.join(start_path, file_to_find)
        if os.path.exists(possible):
            return possible
        elif start_path == os.path.dirname(start_path):
            done = True
        else:
            start_path = os.path.dirname(start_path)
    return None
            
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
    parser.add_option('--coverage-file')
    parser.add_option('--python-file', help='specify Python file to analyze')

    group = optparse.OptionGroup(parser, "Flymake mode")
    group.add_option('--flymake', action='store_true', help='spit out flymake compatible output (requires --python-file)')
    parser.add_option_group(group)

    opt, args = parser.parse_args(prog_args)

    c2e = None
    if opt.coverage_file:
        c2e = Coverage2Emacs(opt.coverage_file)
    elif opt.python_file:
        cov = find_coverage_file(opt.python_file)
        if cov:
            c2e = Coverage2Emacs(cov)
    if c2e is None:
        home_dir = os.path.expanduser('~')
        c2e = Coverage2Emacs(os.path.join(home_dir, '.coverage'))

    if opt.flymake:
        c2e.to_emacs_flymake_mode(opt.python_file)
    else:
        c2e.to_emacs_compile_mode()
        
if __name__ == '__main__':
    sys.exit(main(sys.argv))

