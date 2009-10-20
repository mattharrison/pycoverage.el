import os

def get_coverage_for_function(self, function, startfile):
    # get tests for function
    tf = TestFinder(function, startfile)
    tests = tf.candidate_files(walk_callback)
    
    # run tests with coverage
    
    


class TestFinder(object):
    """
    Dumb code to find function references
    >>> tf = TestFinder('add_argument', '../../pylibs/argparse/argparse.py')
    >>> [x for x in tf.candidate_files(walk_callback)]
    """
    def __init__(self, function_name, start_file):
        self.function_name = function_name
        self.start_file = start_file

    def find_references(self):
        for cand_file in self.candidate_files():
            pass

    def candidate_files(self, callback):
        # start from start_file, work up to setup.py
        
        parent_dir = os.path.dirname(self.start_file)

        done = False
        while not done:
            files = os.listdir(parent_dir)
            if 'setup.py' in files:
                done = True
            elif os.path.dirname(parent_dir) == parent_dir:
                # at the root
                done = True
            else:
                parent_dir = os.path.dirname(parent_dir)

        test_files = []
        os.path.walk(parent_dir, callback, test_files)
        for filename in test_files:
            for filename, classname, methodname, line_num in self.func_name_found(filename):
                yield filename, classname, methodname, line_num
        
    def func_name_found(self, filename):
        classname = None
        methodname = None
        prev = None
        for line_num, line in enumerate(open(filename)):
            classname = _get_class(line, classname)
            methodname = _get_function(line, methodname)
            if self.function_name in line:
                if prev == (filename, classname, methodname):
                    # ignore same method
                    continue
                yield filename, classname, methodname, line_num+1
                prev = filename, classname, methodname
                
def _get_function(line, cur_func):
    """
    >>> _get_function('foo', None)
    >>> _get_function('def hello():', None)
    'hello'
    """
    if 'def ' in line:
        start = line.find('def ') + len('def ')
        end = line.find('(')
        return line[start:end]
    return cur_func

def _get_class(line, cur_class):
    if 'class ' in line and '):' in line:
        start = line.find('class ') + len('class ')
        end = line.find('(')
        return line[start:end]
    return cur_class

                
def walk_callback(keep, dirname, fnames):
    """
    return any file that has test in it's filename (including
    directories)
    """
    for ignore in ['.svn']:
        if ignore in dirname:
            return
    if 'test' in dirname:
        for fname in fnames:
            full_name = os.path.join(dirname, fname)
            if os.path.isfile(full_name):
                keep.append(full_name)

    else:
        for fname in fnames:
            if 'test' in fname:
                full_name = os.path.join(dirname, fname)
                if os.path.isfile(full_name):
                    keep.append(full_name)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
    #tf = TestFinder('get_from_config_file', '../../pylibs/argparse/argparse.py')
    #print [x for x in tf.candidate_files(walk_callback)]
