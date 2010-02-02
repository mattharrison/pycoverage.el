#!/usr/bin/env python
# Copyright (c) 2009-2010 Matt Harrison

import sys
import optparse
import os
import logging

from coverage.report import Reporter
from coverage.misc import CoverageException
from coverage.control import coverage as cv

import meta
import findtests

COVERED = 'Error'#'Covered'
IGNORED = 'Ignored'
MISSED = 'Missed'

logging.basicConfig(level=logging.DEBUG, filename='.cov2emacs.log')
LOG = logging

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
                #statements, excluded, missing, _ = self.coverage._analyze(cu)
                analysis_instance = self.coverage._analyze(cu)
                yield (cu, analysis_instance.statements, analysis_instance.excluded, analysis_instance.missing)
            except KeyboardInterrupt:
                raise
            except CoverageException, e:
                pass
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

            
    def to_emacs_compile_mode(self, fout=None, filenames=None, combine_nums=False):
        """
        spit out something like this that emacs understands

        filename:linenumber:message

        Message can be Covered|Ignored|Missed
        """
        filenames = [os.path.abspath(f) for f in filenames] or []
        LOG.debug('compile_mode filenames: %s' % filenames)
        fout = fout or sys.stdout
        reporter = BasicReporter(self.cov_file)
        # covert the report output to a more useful generator
        data_iter = ((cu.filename, missing, 'MISSING') for cu, statements, excluded, missing in reporter.report())
        filtered_names = self.filter_old_files(data_iter)
        for filename, line, status in filtered_names:
            #print "F", filename, filenames
            if filenames and filename not in filenames:
                continue
            if combine_nums: 
                for line_chunk in combine_linenums(line):
                    fout.write('%s:%s:%s\n' %(filename, line_chunk, status))
            else:
                for num in line:
                    fout.write('%s:%s:%s\n' %(filename, num, status))
    
    def filter_old_files(self, data_iter):
        cov_date = os.stat(self.cov_file).st_mtime
        LOG.debug("FILTER COV MTIME %s " % cov_date)
        file_date = None
        prev_file = None
        for filename, line, status in data_iter:
            if prev_file is None or prev_file != filename:
                file_date = os.stat(filename).st_mtime

            if file_date > cov_date:
                LOG.debug("FILTERING %s date: %s" % (filename, file_date))
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
        #LOG.debug('looking for coverage in %s' % start_path)
        possible = os.path.join(start_path, file_to_find)
        LOG.debug('looking for coverage in (%s)' % possible)
        if os.path.exists(possible):
            LOG.debug('coverage file:%s' % possible)
            return possible
        elif start_path == os.path.dirname(start_path):
            LOG.debug("1")
            done = True
        else:
            LOG.debug("2")
            start_path = os.path.dirname(start_path)
    LOG.debug('no coverage file!')
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

    compile_group = optparse.OptionGroup(parser, "Compile mode")
    group.add_option('--compile-mode', action='store_true', help='spit out compile compatible output')
    parser.add_option_group(compile_group)

    func_group = optparse.OptionGroup(parser, "Run a function with coverage")
    func_group.add_option('--function-name', help='Report on coverage for tests for this function (requires --python-file)')
    parser.add_option_group(func_group)
    
    opt, args = parser.parse_args(prog_args)

    if opt.flymake or opt.compile_mode:
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

    if opt.function_name:
        findtests.get_coverage_for_function(opt.function_name, opt.python_file)
        cov = find_coverage_file(opt.python_file)
        c2e = Coverage2Emacs(cov)

    if opt.flymake:
        c2e.to_emacs_flymake_mode(opt.python_file)
    elif opt.compile_mode:
        c2e.to_emacs_compile_mode(filenames=[opt.python_file])
        
if __name__ == '__main__':
    sys.exit(main(sys.argv))

