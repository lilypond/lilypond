#!@PYTHON@

# 
# flower.py -- python flower lib
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 

class File:
    """silly wrapper for Python file object."""
    def __init__ (self,nm, mode='r'):
	if nm:
	    self.file_ = open (nm, mode);
	elif mode == 'w':
	    self.file_ = sys.stdout
	else:
	    self.file_ = sys.stdin
	    
	self.eof_ = 0;
    def readline (self):
    	l=  self.file_.readline ();
	if not l:
	    self.eof_ = 1;
	return l;
    def write (self, str):
    	self.file_.write (str)
    def eof (self):
	return self.eof_
    def close (self):
    	self.file_.close ()
    def __del__ (self):
    	self.close ();



import fnmatch
import os

_debug = 0

_prune = ['(*)']


def my_find(patterns, dir = os.curdir):
        list = []
        names = os.listdir(dir)
        names.sort()
        for name in names:
                if name in (os.curdir, os.pardir):
                        continue
                fullname = os.path.join(dir, name)
		for pat in patterns:
		    if fnmatch.fnmatch(name, pat):
                        list.append(fullname)
                if os.path.isdir(fullname) and not os.path.islink(fullname):
                        for p in _prune:
                                if fnmatch.fnmatch(name, p):
                                        if _debug: print "skip", `fullname`
                                        break
                        else:
                                if _debug: print "descend into", `fullname`
				found = my_find(patterns, fullname)
				if found:
				    list = list + found
        return list

def multiple_find(pats, dirnames):
    from find import find
    l = []
    for d in dirnames:
	l = l + my_find(pats,  d)
    return l
#!@PYTHON@

# 
# flower.py -- python flower lib
# 
# source file of the GNU LilyPond music typesetter
# 
# (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
# 

class File:
    """silly wrapper for Python file object."""
    def __init__ (self,nm, mode='r'):
	if nm:
	    self.file_ = open (nm, mode);
	elif mode == 'w':
	    self.file_ = sys.stdout
	else:
	    self.file_ = sys.stdin
	    
	self.eof_ = 0;
    def readline (self):
    	l=  self.file_.readline ();
	if not l:
	    self.eof_ = 1;
	return l;
    def write (self, str):
    	self.file_.write (str)
    def eof (self):
	return self.eof_
    def close (self):
    	self.file_.close ()
    def __del__ (self):
    	self.close ();



import fnmatch
import os

_debug = 0

_prune = ['(*)']


def my_find(patterns, dir = os.curdir):
        list = []
	try:
	    names = os.listdir(dir)
	except os.error:
	    names = []
        names.sort()
        for name in names:
                if name in (os.curdir, os.pardir):
                        continue
                fullname = os.path.join(dir, name)
		for pat in patterns:
		    if fnmatch.fnmatch(name, pat):
                        list.append(fullname)
                if os.path.isdir(fullname) and not os.path.islink(fullname):
                        for p in _prune:
                                if fnmatch.fnmatch(name, p):
                                        if _debug: print "skip", `fullname`
                                        break
                        else:
                                if _debug: print "descend into", `fullname`
				found = my_find(patterns, fullname)
				if found:
				    list = list + found
        return list

def multiple_find(pats, dirnames):
    from find import find
    l = []
    for d in dirnames:
	l = l + my_find(pats,  d)
    return l
