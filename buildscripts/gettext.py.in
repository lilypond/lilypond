"""This module allows python programs to use GNU gettext message catalogs.

Author: James Henstridge <james@daa.com.au>
(This is loosely based on gettext.pl in the GNU gettext distribution)

The best way to use it is like so:
    import gettext
    gettext.bindtextdomain(PACKAGE, LOCALEDIR)
    gettext.textdomain(PACKAGE)
    _ = gettext.gettext
    print _('Hello World')

where PACKAGE is the domain for this package, and LOCALEDIR is usually
'$prefix/share/locale' where $prefix is the install prefix.

If you have more than one catalog to use, you can directly create catalog
objects.  These objects are created as so:
    import gettext
    cat = gettext.Catalog(PACKAGE, localedir=LOCALEDIR)
    _ = cat.gettext
    print _('Hello World')

The catalog object can also be accessed as a dictionary (ie cat['hello']).

There are also some experimental features.  You can add to the catalog, just
as you would with a normal dictionary.  When you are finished, you can call
its save method, which will create a new .mo file containing all the
translations:
    import gettext
    cat = Catalog()
    cat['Hello'] = 'konichiwa'
    cat.save('./tmp.mo')

Once you have written an internationalized program, you can create a .po file
for it with "xgettext --keyword=_ fillename ...".  Then do the translation and
compile it into a .mo file, ready for use with this module.  Note that you
will have to use C style strings (ie. use double quotes) for proper string
extraction.
"""
import os, string

prefix = '/usr/local'
localedir = prefix + '/share/locale'

def _expandLang(str):
	langs = [str]
	# remove charset ...
	if '.' in str:
		langs.append(string.split(str, '.')[0])
	# also add 2 character language code ...
	if len(str) > 2:
		langs.append(str[:2])
	return langs

lang = []
for env in 'LANGUAGE', 'LC_ALL', 'LC_MESSAGES', 'LANG':
	if os.environ.has_key(env):
		lang = string.split(os.environ[env], ':')
		lang = map(_expandLang, lang)
		lang = reduce(lambda a, b: a + b, lang)
		break
if 'C' not in lang:
	lang.append('C')

# remove duplicates
i = 0
while i < len(lang):
	j = i + 1
	while j < len(lang):
		if lang[i] == lang[j]:
			del lang[j]
		else:
			j = j + 1
	i = i + 1
del i, j

if os.environ.has_key('PY_XGETTEXT'):
	xgettext = os.environ['PY_XGETTEXT']
else:
	xgettext = None

del os, string

error = 'gettext.error'

def _lsbStrToInt(str):
	return ord(str[0]) + \
	       (ord(str[1]) << 8) + \
	       (ord(str[2]) << 16) + \
	       (ord(str[3]) << 24)
def _msbStrToInt(str):
	return (ord(str[0]) << 24) + \
	       (ord(str[1]) << 16) + \
	       (ord(str[2]) << 8) + \
	       ord(str[3])
def _intToLsbStr(int):
	return chr(int         & 0xff) + \
	       chr((int >> 8)  & 0xff) + \
	       chr((int >> 16) & 0xff) + \
	       chr((int >> 24) & 0xff)

def _getpos(levels = 0):
	"""Returns the position in the code where the function was called.
	The function uses some knowledge about python stack frames."""
	import sys
	# get access to the stack frame by generating an exception.
	try:
		raise RuntimeError
	except RuntimeError:
		frame = sys.exc_traceback.tb_frame
	frame = frame.f_back # caller's frame
	while levels > 0:
		frame = frame.f_back
		levels = levels - 1
	return (frame.f_globals['__name__'],
		frame.f_code.co_name,
		frame.f_lineno)

class Catalog:
	def __init__(self, domain=None, localedir=localedir):
		self.domain = domain
		self.localedir = localedir
		self.cat = {}
		if not domain: return
		for self.lang in lang:
			if self.lang == 'C':
				return
			catalog = "%s//%s/LC_MESSAGES/%s.mo" % (
				localedir, self.lang, domain)
			try:
				f = open(catalog, "rb")
				buffer = f.read()
				del f
				break
			except IOError:
				pass
		else:
			return # assume C locale

		strToInt = _lsbStrToInt
		if strToInt(buffer[:4]) != 0x950412de:
			# catalog is encoded with MSB offsets.
			strToInt = _msbStrToInt
			if strToInt(buffer[:4]) != 0x950412de:
				# magic number doesn't match
				raise error, 'Bad magic number in %s' % (catalog,)

		self.revision = strToInt(buffer[4:8])
		nstrings = strToInt(buffer[8:12])
		origTabOffset  = strToInt(buffer[12:16])
		transTabOffset = strToInt(buffer[16:20])
		for i in range(nstrings):
			origLength = strToInt(buffer[origTabOffset:
						     origTabOffset+4])
			origOffset = strToInt(buffer[origTabOffset+4:
						     origTabOffset+8])
			origTabOffset = origTabOffset + 8
			origStr = buffer[origOffset:origOffset+origLength]
		
			transLength = strToInt(buffer[transTabOffset:
						      transTabOffset+4])
			transOffset = strToInt(buffer[transTabOffset+4:
						      transTabOffset+8])
			transTabOffset = transTabOffset + 8
			transStr = buffer[transOffset:transOffset+transLength]
			
			self.cat[origStr] = transStr

	def gettext(self, string):
		"""Get the translation of a given string"""
		if self.cat.has_key(string):
			return self.cat[string]
		else:
			return string
	# allow catalog access as cat(str) and cat[str] and cat.gettext(str)
	__getitem__ = gettext
	__call__ = gettext

	# this is experimental code for producing mo files from Catalog objects
	def __setitem__(self, string, trans):
		"""Set the translation of a given string"""
		self.cat[string] = trans
	def save(self, file):
		"""Create a .mo file from a Catalog object"""
		try:
			f = open(file, "wb")
		except IOError:
			raise error, "can't open " + file + " for writing"
		f.write(_intToLsbStr(0x950412de))    # magic number
		f.write(_intToLsbStr(0))             # revision
		f.write(_intToLsbStr(len(self.cat))) # nstrings

		oIndex = []; oData = ''
		tIndex = []; tData = ''
		for orig, trans in self.cat.items():
			oIndex.append((len(orig), len(oData)))
			oData = oData + orig + '\0'
			tIndex.append((len(trans), len(tData)))
			tData = tData + trans + '\0'
		oIndexOfs = 20
		tIndexOfs = oIndexOfs + 8 * len(oIndex)
		oDataOfs = tIndexOfs + 8 * len(tIndex)
		tDataOfs = oDataOfs + len(oData)
		f.write(_intToLsbStr(oIndexOfs))
		f.write(_intToLsbStr(tIndexOfs))
		for length, offset in oIndex:
			f.write(_intToLsbStr(length))
			f.write(_intToLsbStr(offset + oDataOfs))
		for length, offset in tIndex:
			f.write(_intToLsbStr(length))
			f.write(_intToLsbStr(offset + tDataOfs))
		f.write(oData)
		f.write(tData)

_cat = None
_cats = {}

if xgettext:
	class Catalog:
		def __init__(self, domain, localedir):
			self.domain = domain
			self.localedir = localedir
			self._strings = {}
		def gettext(self, string):
			# there is always one level of redirection for calls
			# to this function
			pos = _getpos(2) # get this function's caller
			if self._strings.has_key(string):
				if pos not in self._strings[string]:
					self._strings[string].append(pos)
			else:
				self._strings[string] = [pos]
			return string
		__getitem__ = gettext
		__call__ = gettext
		def __setitem__(self, item, data):
			pass
		def save(self, file):
			pass
		def output(self, fp):
			import string
			fp.write('# POT file for domain %s\n' % (self.domain,))
			for str in self._strings.keys():
				pos = map(lambda x: "%s(%s):%d" % x,
					  self._strings[str])
				pos.sort()
				length = 80
				for p in pos:
					if length + len(p) > 74:
						fp.write('\n#:')
						length = 2
					fp.write(' ')
					fp.write(p)
					length = length + 1 + len(p)
				fp.write('\n')
				if '\n' in str:
					fp.write('msgid ""\n')
					lines = string.split(str, '\n')
					lines = map(lambda x:
						    '"%s\\n"\n' % (x,),
						    lines[:-1]) + \
						    ['"%s"\n' % (lines[-1],)]
					fp.writelines(lines)
				else:
					fp.write('msgid "%s"\n' % (str,))
				fp.write('msgstr ""\n')
				
	import sys
	if hasattr(sys, 'exitfunc'):
		_exitchain = sys.exitfunc
	else:
		_exitchain = None
	def exitfunc(dir=xgettext, _exitchain=_exitchain):
		# actually output all the .pot files.
		import os
		for file in _cats.keys():
			fp = open(os.path.join(dir, file + '.pot'), 'w')
			cat = _cats[file]
			cat.output(fp)
			fp.close()
		if _exitchain: _exitchain()
	sys.exitfunc = exitfunc
	del sys, exitfunc, _exitchain, xgettext

def bindtextdomain(domain, localedir=localedir):
	global _cat
	if not _cats.has_key(domain):
		_cats[domain] = Catalog(domain, localedir)
	if not _cat: _cat = _cats[domain]

def textdomain(domain):
	global _cat
	if not _cats.has_key(domain):
		_cats[domain] = Catalog(domain)
	_cat = _cats[domain]

def gettext(string):
	if _cat == None: raise error, "No catalog loaded"
	return _cat.gettext(string)

_ = gettext

def dgettext(domain, string):
	if domain is None:
		return gettext(string)
	if not _cats.has_key(domain):
		raise error, "Domain '" + domain + "' not loaded"
	return _cats[domain].gettext(string)

def test():
	import sys
	global localedir
	if len(sys.argv) not in (2, 3):
		print "Usage: %s DOMAIN [LOCALEDIR]" % (sys.argv[0],)
		sys.exit(1)
	domain = sys.argv[1]
	if len(sys.argv) == 3:
		bindtextdomain(domain, sys.argv[2])
	textdomain(domain)
	info = gettext('')  # this is where special info is often stored
	if info:
		print "Info for domain %s, lang %s." % (domain, _cat.lang)
		print info
	else:
		print "No info given in mo file."

if __name__ == '__main__':
	test()

