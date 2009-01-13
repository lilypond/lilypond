#! @PYTHON@

import os
import sys

def relative (dir, r=os.getcwd ()):
    dir = os.path.normpath (dir) + '/'
    r = os.path.normpath (r) + '/'
    prefix = len (os.path.commonprefix ((dir, r)))
    if prefix == len (dir):
        return './'
    if prefix > 1:
        return '../' * len (r[prefix:].split ('/')) + dir[prefix:]
    return dir

if __name__ == '__main__':
    print relative (sys.argv[1])
