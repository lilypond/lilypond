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

