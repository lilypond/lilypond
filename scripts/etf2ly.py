#!@PYTHON@

# info taken from

#  * convertmusic (see sourceforge)
#  * Margaret Cahill's thesis
#  * http://www.codamusic.com/coda/Fin2000_pdk_download.asp (you have
#    to register, but you don't need to have bought a code product

#
# This supports

#
#  * notes
#  * rests
#  * ties
#  * slurs
#

# todo:
#  * slur/stem directions
#  * articulation
#  * voices (2nd half of frame?)
#  * lyrics
#  * beams (better use autobeam?)

program_name = 'etf2ly'
version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
	version = '(unknown version)'	   # uGUHGUHGHGUGH
  
import __main__
import getopt
import sys
import re
import string
import os



finale_clefs= ['treble', 'alto', 'tenor', 'bass', 'percussion', 'treble8vb', 'bass8vb', 'baritone']


def lily_clef (fin):
	return finale_clefs[fin]

def gulp_file(f):
	return open (f).read ()

# notename 0 == central C
distances = [0, 2, 4, 5, 7, 9, 11, 12]
def semitones (name, acc):
	return (name / 7 ) * 12 + distances[name % 7] + acc

def transpose(orig, delta):
	(oname, oacc) = orig
	(dname, dacc) = delta
	
	old_pitch =semitones (oname, oacc)
	delta_pitch = semitones (dname, dacc)
	nname = (oname + dname) 
	nacc = oacc
	new_pitch = semitones (nname, nacc) 

	nacc = nacc - (new_pitch - old_pitch - delta_pitch)

	return (nname, nacc)



# find transposition of C-major scale that belongs here. 
def interpret_finale_key_sig (finale_id):
	p = (0,0)
	if 0 <= finale_id < 7:
		while finale_id > 0:
			p = transpose (p, (4,0)) # a fifth up
			finale_id = finale_id - 1
	elif 248 < finale_id <= 255:
		while finale_id < 256:
			p = transpose (p, (3,0))
			finale_id = finale_id + 1

	p  = (p[0] % 7, p[1])
	return p

# should cache this.
def find_scale (transposition):
	cscale = map (lambda x: (x,0), range (0,7))
	trscale = map(lambda x, k=transposition: transpose(x, k), cscale)

	return trscale

def gcd (a,b):
	if b == 0:
		return a
	c = a
	while c: 
		c = a % b
		a = b
		b = c
	return a
	

def rat_simplify (r):
	(n,d) = r
	if d < 0:
		d = -d
		n = -n
	if n == 0:
		return (0,1)
	else:
		g = gcd (n, d)
		return (n/g, d/g)
	
def rat_multiply (a,b):
	(x,y) = a
	(p,q) = b

	return rat_simplify ((x*p, y*q))

def rat_add (a,b):
	(x,y) = a
	(p,q) = b

	return rat_simplify ((x*q + p*y, y*q))

def rat_neg (a):
	(p,q) = a
	return (-p,q)

def rat_subtract (a,b ):
	return rat_add (a, rat_neg (b))

def lily_notename (tuple2):
	(n, a) = tuple2
	nn = chr ((n+ 2)%7 + ord ('a'))

	if a == -1:
		nn = nn + 'es'
	elif a == -2:
		nn = nn + 'eses'
	elif a == 1:
		nn = nn + 'is'
	elif a == 2:
		nn = nn + 'isis'

	return nn

class Slur:
	def __init__ (self, number):
		self.number = number
		self.finale = []

	def append_entry (self, finale_e):
		self.finale.append (finale_e)

	def calculate (self, chords):
		startnote = self.finale[0][5]
		endnote = self.finale[3][2]

		cs = chords[startnote]
		cs.suffix = '(' + cs.suffix 
		ce = chords[endnote]
		ce.prefix = ce.prefix + ')'
		
class Global_measure:
	def __init__ (self, number):
		self.timesig = ''
		self.number = number
		self.keysignature = None
		self.scale = None

		self.finale = []

	def set_timesig (self, finale):
		(beats, fdur) = finale
		(log, dots) = EDU_to_duration (fdur)
		assert dots == 0
		self.timesig = (beats, log)

	def length (self):
		return self.timesig
	
	def set_keysig (self, finale):
		k = interpret_finale_key_sig (finale)
		self.keysignature = k
		self.scale = find_scale (k)



class Measure:
	def __init__(self, no):
		self.number = no
		self.frames = []
		self.flags = 0
		self.clef = 0
		self.finale = []
		self.global_measure = None
		
	def add_finale_entry (self, entry):
		self.finale.append (entry)

	def calculate (self):
		f0  = self.finale[0]
		f1 = self.finale[1]
		
		self.clef = string.atoi (f0[0])
		self.flags = string.atoi (f0[1])
		fs = map (string.atoi, list (f0[2:]) + [f1[0]])

		self.frames = fs

class Frame:
	def __init__ (self, finale):
		self.measure = None
		self.finale = finale
		(number, start, end ) = finale
		self.number = number
		self.start = start
		self.end = end
		self.chords  = []

	def set_measure (self, m):
		self.measure = m

	def dump (self):
		str = ''
		left = self.measure.global_measure.length ()
		for c in self.chords:
			str = str + c.ly_string () + ' '
			left = rat_subtract (left, c.length ())
		if left[0] < 0:
			print self.number
			print self.start, self.end
			print left
			raise 'bla'
		if left[0]:
			str = str + 's*%d/%d' % left
			
		str = str + '\n'
		return str
		
class Staff:
	def __init__ (self, number):
		self.number = number
		self.measures = []

	def get_measure (self, no):
		if len (self.measures) <= no:
			self.measures = self.measures + [None]* (1 + no - len (self.measures))

		if self.measures[no] == None:
			self.measures [no] = Measure (no)

		return self.measures[no]
	def staffid (self):
		return 'staff%s'% chr (self.number - 1 +ord ('A'))
	def layerid (self, l):
		return self.staffid() +  'layer%s' % chr (l -1 + ord ('A'))
	
	def dump_time_key_sigs (self):
		k  = ''
		last_key = None
		last_time = None
		last_clef = None
		gap = (0,1)
		for m in self.measures[1:]:
			g = m.global_measure
			e = ''
			if last_key <> g.keysignature:
				e = e + "\\key %s \\major; " % lily_notename (g.keysignature)
				last_key = g.keysignature
			if last_time <> g.timesig :
				e = e + "\\time %d/%d; " % g.timesig
				last_time = g.timesig
			if last_clef <> m.clef :
				e = e + '\\clef %s;' % lily_clef (m.clef)
				last_clef = m.clef
			if e:
				if gap <> (0,1):
					k = k +' s1*%d/%d \n ' % gap
				gap = (0,1)
				k = k + e
			
			gap = rat_add (gap, g.length ())

				
		k = '%sglobal = \\notes  { %s }\n\n ' % (self.staffid (), k)
		return k
	
	def dump (self):
		str = ''


		layerids = []
		for x in range (1,5): # 4 layers.
			laystr =  ''
			last_frame = None
			first_frame = None
			gap = (0,1)
			for m in self.measures[1:]:
				fr = m.frames[x]
				if fr:
					first_frame = fr
					if gap <> (0,1):
						laystr = laystr +'} s1*%d/%d {\n ' % gap
						gap = (0,1)
					laystr = laystr + fr.dump ()
				else:
					gap = rat_add (gap, m.global_measure.length ())

			if first_frame:
				l = self.layerid (x)
				laystr = '%s =  \\notes { { %s } }\n\n' % (l, laystr)
				str = str  + laystr
				layerids.append (l)

		str = str +  self.dump_time_key_sigs ()		
		stafdef = '\\%sglobal' % self.staffid ()
		for i in layerids:
			stafdef = stafdef + ' \\' + i
			

		str = str + '%s = \\context Staff = %s <\n %s\n >\n' % \
		      (self.staffid (), self.staffid (), stafdef)
		return str

				
def EDU_to_duration (edu):
	log = 1
	d = 4096
	while d > edu:
		d = d >> 1
		log = log << 1

	edu = edu - d
	dots = 0
	if edu == d /2:
		dots = 1
	elif edu == d*3/4:
		dots = 2
	return (log, dots)	

class Chord:
	def __init__ (self, finale_entry):
		self.pitches = []
		self.frame = None
		self.finale = finale_entry
		self.duration  = None
		self.next = None
		self.prev = None
		self.prefix= ''
		self.suffix = ''
	def measure (self):
		if not self.frame:
			return None
		return self.frame.measure

	def length (self):
		l = (1, self.duration[0])

		d = 1 << self.duration[1]

		dotfact = rat_subtract ((2,1), (1,d))
		return rat_multiply (dotfact, l)
		
	def number (self):
		return self.finale[0][0]
	def set_duration (self):
		((no, prev, next, dur, pos, entryflag, extended, follow),
		 notelist) = self.finale
		self.duration = EDU_to_duration(dur)
	def find_realpitch (self):
		
		((no, prev, next, dur, pos, entryflag, extended, follow), notelist) = self.finale

		meas = self.measure ()
		tiestart = 0
		if not meas or not meas.global_measure  :
			print 'note %d not in measure' % self.number ()
		elif not meas.global_measure.scale:
			print  'note %d: no scale in this measure.' % self.number ()
		else:
			for p in notelist:
				(pitch, flag) = p
				
				nib1 = pitch & 0x0f
				if nib1 > 8:
					nib1 = -(nib1 - 8)
				rest = pitch / 16

				scale =  meas.global_measure.scale 
				(sn, sa) =scale[rest % 7]
				sn = sn + (rest - (rest%7)) + 7
				acc = sa + nib1
				self.pitches.append ((sn, acc))
				tiestart =  tiestart or (flag & Chord.TIE_START_MASK)
		if tiestart :
			self.suffix = self.suffix + ' ~ '
		
	REST_MASK = 0x40000000L
	TIE_START_MASK = 0x40000000L
	def ly_string (self):
		s = ''

		rest = ''

		if not (self.finale[0][5] & Chord.REST_MASK):
			rest = 'r'
		
		for p in self.pitches:
			(n,a) =  p
			o = n/ 7
			n = n % 7

			nn = lily_notename ((n,a))

			if o < 0:
				nn = nn + (',' * -o)
			elif o > 0:
				nn = nn + ('\'' * o)
				
			if s:
				s = s + ' '

			if rest:
				nn = rest
				
			s = s + '%s%d%s' % (nn, self.duration[0], '.'* self.duration[1])
			
		if len (self.pitches) > 1:
			s = '<%s>' % s
		elif not self.pitches:
			s  = 'r%d%s' % (self.duration[0] , '.'* self.duration[1])

		s = self.prefix + s + self.suffix
		return s

GFre = re.compile(r"""^\^GF\(([0-9-]+),([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
BCre = re.compile (r"""^\^BC\(([0-9-]+)\) ([0-9-]+) .*$""")
eEre = re.compile(r"""^\^eE\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) \$([0-9A-Fa-f]+) ([0-9-]+) ([0-9-]+)""")
FRre = re.compile (r"""^\^FR\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
MSre = re.compile (r"""^\^MS\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
note_re = re.compile (r"""^ +([0-9-]+) \$([A-Fa-f0-9]+)""")
Sxre  = re.compile (r"""^\^Sx\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")

class Etf_file:
	def __init__ (self, name):
		self.measures = [None]
		self.entries = [None]
		self.chords = [None]
		self.frames = [None]
		self.staffs = [None]
		self.slurs = [None]

		## do it
		self.parse (name)

	def get_global_measure (self, no):
		if len (self.measures) <= no:
			self.measures = self.measures + [None]* (1 + no - len (self.measures))

		if self.measures[no] == None:
			self.measures [no] = Global_measure (no)

		return self.measures[no]

		
	def get_staff(self,staffno):
		if len (self.staffs) <= staffno:
			self.staffs = self.staffs + [None] * (1 + staffno - len (self.staffs))

		if self.staffs[staffno] == None:
			self.staffs[staffno] = Staff (staffno)

		return self.staffs[staffno]

	# staff-spec
	def try_IS (self, l):
		pass

	def try_BC (self, l):
		m =  BCre.match  (l)
		if m:
			bn = string.atoi (m.group (1))
			where = string.atoi (m.group (2)) / 1024.0
		return m

	def try_eE (self, l):
		m = eEre.match (l)
		if m:
			tup = m.groups()
			(no, prev, next, dur, pos, entryflag, extended, follow) = tup
			(no, prev, next, dur, pos,extended, follow) \
			  = tuple (map (string.atoi, [no,prev,next,dur,pos,extended,follow]))

			entryflag = string.atol (entryflag,16)
			assert (no==len (self.entries))
			current_entry = ((no, prev, next, dur, pos, entryflag, extended, follow), [])
			self.entries.append (current_entry)
		return m

	def try_Sx(self,l):
		m = Sxre.match (l)
		if m:
			slurno = string.atoi (m.group (1))

			if len (self.slurs) == slurno:
				self.slurs.append (Slur (slurno))

			params = list (m.groups ()[1:])
			params = map (string.atoi, params)
			self.slurs[-1].append_entry (params)

		return m	
	def try_GF(self, l):
		m = GFre.match (l)
		if m:
			(staffno,measno) = m.groups ()[0:2]
			s = string.atoi (staffno)
			me = string.atoi (measno)
			
			entry = m.groups () [2:]
			st = self.get_staff (s)
			meas = st.get_measure (me)
			meas.add_finale_entry (entry)
		
	# frame  ?
	def try_FR(self, l):
		m = FRre.match (l)
		if m:
			(frameno, startnote, endnote, foo, bar) = m.groups ()
			(frameno, startnote, endnote)  = tuple (map (string.atoi, [frameno, startnote, endnote]))
			self.frames.append (Frame ((frameno, startnote, endnote)))
			
		return m
	def try_MS (self, l):
		m = MSre.match (l)
		if m:
			measno = string.atoi (m.group (1))
			keynum = string.atoi (m.group (3))
			meas =self. get_global_measure (measno)
			meas.set_keysig (keynum)

			beats = string.atoi (m.group (4))
			beatlen = string.atoi (m.group (5))
			meas.set_timesig ((beats, beatlen))
						
		return m

	def try_note (self, l):
		m = note_re.match (l)
		if m:
			(pitch, flag) = m.groups ()
			pitch = string.atoi (pitch)
			flag = string.atol (flag,16)
			self.entries[-1][1].append ((pitch,flag))

	def parse (self, name):
		sys.stderr.write ('parsing ...')
		sys.stderr.flush ()
		
		ls = open (name).readlines ()
		for l in ls:
			m = None
			if not m: 
				m = self.try_MS (l)
			if not m: 
				m = self.try_FR (l)
			if not m: 
				m = self.try_GF (l)
			if not m: 
				m = self.try_note (l)
			if not m: 
				m = self.try_eE (l)
			if not m:
				m = self.try_Sx (l)

		sys.stderr.write ('processing ...')
		sys.stderr.flush ()

		self.unthread_entries ()

		for st in self.staffs[1:]:
			if not st:
				continue
			mno = 1
			for m in st.measures[1:]:
				m.global_measure = self.measures[mno]
				m.calculate()

				frame_obj_list = [None]
				for frno in m.frames:
					fr = self.frames[frno]
					frame_obj_list.append (fr)

				m.frames = frame_obj_list
				for fr in frame_obj_list[1:]:
					if not fr:
						continue
					
					fr.set_measure (m)
					
					fr.chords = self.get_thread (fr.start, fr.end)
					for c in fr.chords:
						c.frame = fr
				mno = mno + 1

		for c in self.chords[1:]:
			c.find_realpitch ()
			c.set_duration ()
			
		for s in self.slurs [1:]:
			s.calculate (self.chords)
			
	def get_thread (self, startno, endno):

		thread = []
		c = self.chords[startno]
		while c and c.number () <> endno:
			thread.append (c)
			c = c.next

		if c: 
			thread.append (c)
		
		return thread

	def dump (self):
		str = ''
		staffs = []
		for s in self.staffs[1:]:
			if s:
				str = str + '\n\n' + s.dump () 
				staffs.append ('\\' + s.staffid ())

		if staffs:
			str = str + '\\score { < %s > } ' % string.join (staffs)

		
		return str


	def __str__ (self):
		return self.dump ()
	
	def unthread_entries (self):
		self.chords = [None]
		for e in self.entries[1:]:
			self.chords.append (Chord (e))

		for e in self.chords[1:]:
			e.prev = self.chords[e.finale[0][1]]
			e.next = self.chords[e.finale[0][2]]


	 



def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))

def help ():
	print r"""
Convert ETF to LilyPond.

Usage: etf2ly [OPTION]... ETF-FILE

Options:
  -h, --help          this help
  -o, --output=FILE   set output filename to FILE
  -v, --version       version information

Enigma Transport Format is a format used by Coda Music Technology's
Finale product. This program will convert a subset of ETF to a
ready-to-use lilypond file.


"""

def print_version ():
	print r"""etf2ly (GNU lilypond) %s""" % version



(options, files) = getopt.getopt (sys.argv[1:], 'vo:h', ['help','version', 'output='])
out_filename = None

for opt in options:
	o = opt[0]
	a = opt[1]
	if o== '--help' or o == '-h':
		help ()
		sys.exit (0)
	if o == '--version' or o == '-v':
		print_version ()
		sys.exit(0)
		
	if o == '--output' or o == '-o':
		out_filename = a
	else:
		print o
		raise getopt.error

identify()

# header['tagline'] = 'Lily was here %s -- automatically converted from ABC' % version
for f in files:
	if f == '-':
		f = ''

	sys.stderr.write ('Processing `%s\'\n' % f)
	e = Etf_file(f)
	if not out_filename:
		out_filename = os.path.basename (re.sub ('.etf$', '.ly', f))
		
	if out_filename == f:
		out_filename = os.path.basename (f + '.ly')
		
	sys.stderr.write ('Writing `%s\'' % out_filename)
	ly = e.dump()

	
	
	fo = open (out_filename, 'w')
	fo.write ('%% lily was here -- automatically converted by etf2ly from %s\n' % f)
	fo.write(ly)
	fo.close ()
	
