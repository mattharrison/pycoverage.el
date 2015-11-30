#!/usr/bin/env python
# Copyright (c) 2009-2010 Matt Harrison

import sys
import optparse
import os
import logging

from coverage.report import Reporter
from coverage.misc import CoverageException
from coverage.control import Coverage
from coverage.config import CoverageConfig

import meta


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
        coverage = Coverage(report_file)
        coverage.use_cache(True)
        coverage.load()
        self.config = CoverageConfig()

        super(BasicReporter, self).__init__(coverage, ignore_errors)

    def report_filenames(self, filenames=None):
        filenames = filenames or []
        for filename in filenames:
            yield self.coverage.analysis(filename)

    def report(self, morfs=None, directory=None):
        for result in self.report_files(morfs, directory):
            yield result

    def report_files(self, morfs, directory=None):
        """Run a reporting function on a number of morfs.

        No callback function, just yield the cu, statements, excluded and missing
        """
        self.find_code_units(morfs, self.config)

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

    def to_emacs_compile_mode(self, fout=None, filenames=None, combine_nums=False,
                              status_line=True):
        """
        spit out something easy to parse in emacs ie:

        filename:linenumber:message

        Message can be Covered|Ignored|Missed
        """
        filenames = [os.path.abspath(f) for f in filenames] or []
        LOG.debug('compile_mode filenames: %s' % filenames)
        fout = fout or sys.stdout
        reporter = BasicReporter(self.cov_file)
        # convert the report output to a more useful generator
        data_iter = []
        for file, executable_lines, not_executed, summary in reporter.report_filenames(filenames):
            # executable lines are lines that can "run" versus comments/etc
            if len(executable_lines) == 0:
                percent_executed = 100
            else:
                percent_executed = 100*(len(executable_lines) - len(not_executed) + 0.)/len(executable_lines)
            data_iter.append((file, not_executed, 'MISSING', percent_executed))
        filtered_names = self.filter_old_files(data_iter)
        for filename, lines, status, percent in filtered_names:
            if filenames and filename not in filenames:
                continue
            if status == 'OLD':
                fout.write('OLD:?\n')
                # don't bother writing out stale data
                continue
            elif status:
                fout.write('SUCCESS:%d\n' % percent)
            if combine_nums:
                for line_chunk in combine_linenums(lines):
                    fout.write('%s:%s:%s\n' %(filename, line_chunk, status))
            else:
                for num in lines:
                    fout.write('%s:%s:%s\n' %(filename, num, status))



    def filter_old_files(self, data_iter):
        cov_date = os.stat(self.cov_file).st_mtime
        LOG.debug("FILTER COV MTIME %s " % cov_date)
        file_date = None
        prev_file = None
        for data in data_iter:
            filename = data[0]
            if prev_file is None or prev_file != filename:
                file_date = os.stat(filename).st_mtime

            if file_date > cov_date:
                LOG.debug("FILTERING %s date: %s > %s" % (filename, file_date, cov_date))
                # assume that file has been tweeked and data is wrong
                data = list(data)
                data[2] = "OLD"
            yield data
            prev_file = filename

def parent_dirs(start_file):
    """
    find parent dirs
    >>> list(parent_dirs('/usr/lib/python'))
    ['/usr/lib', '/usr', '/']
    """
    start_path = os.path.abspath(start_file)
    start_dir = os.path.dirname(start_path)
    done = False
    while not done:
        yield start_dir
        next_dir = os.path.dirname(start_dir)
        done = next_dir == start_dir
        start_dir = next_dir

def is_older(filename, other_mtime):
    mtime = os.stat(filename).st_mtime
    return mtime > other_mtime

def find_coverage_file(start_file, file_to_find='.coverage'):
    start_mtime = os.stat(start_file).st_mtime
    for parent in parent_dirs(start_file):
        potential = os.path.join(parent, file_to_find)
        LOG.debug('Potential: %s' % potential)
        if not os.path.exists(potential):
            LOG.debug('No file: %s' % potential)
            continue
        if is_older(potential, start_mtime):
            LOG.debug('FOUND! newer: %s' % potential)
            return potential
        else:
            LOG.debug('OLDER: %s' % potential)
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

    parser.add_option('-t', '--run-tests', action='store_true', help='run doctests')
    opt, args = parser.parse_args(prog_args)


    if opt.run_tests:
        _test()
        return
    if opt.flymake or opt.compile_mode:
        c2e = None
        if opt.coverage_file:
            c2e = Coverage2Emacs(opt.coverage_file)
        elif opt.python_file:
            cov = find_coverage_file(opt.python_file)
            if cov:
                c2e = Coverage2Emacs(cov)
        if c2e is None:
            print "NO COVERAGE FILE::"
            return

    if opt.function_name:
        import findtests  # requires nose
        findtests.get_coverage_for_function(opt.function_name, opt.python_file)
        cov = find_coverage_file(opt.python_file)
        c2e = Coverage2Emacs(cov)

    if opt.flymake:
        c2e.to_emacs_flymake_mode(opt.python_file)
    elif opt.compile_mode:
        filenames = []
        if opt.python_file:
            filenames.append(opt.python_file)
        c2e.to_emacs_compile_mode(filenames=filenames)

if __name__ == '__main__':
    sys.exit(main(sys.argv))
