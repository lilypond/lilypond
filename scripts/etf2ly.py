#!@PYTHON@

# info mostly taken from looking at files. See also
# http://www.cs.uu.nl/~hanwen/lily-devel/etf.html

# This supports
#
#  * notes
#  * rests
#  * ties
#  * slurs
#  * lyrics
#  * articulation
#  * grace notes
#  * tuplets


# todo:
#  * slur/stem directions
#  * voices (2nd half of frame?)
#  * more intelligent lyrics
#  * beams (better use autobeam?)
#  * more robust: try entertainer.etf (freenote)
#  * dynamics
#  * automatic `deletion' of invalid items
#


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

finale_clefs= ['treble', 'alto', 'tenor', 'bass', 'percussion', 'treble_8', 'bass_8', 'baritone']

def lily_clef (fin):
	try:
		return finale_clefs[fin]
	except IndexError:
		sys.stderr.write ( '\nHuh? Found clef number %d\n' % fin)

	return 'treble'
	
	

def gulp_file(f):
	return open (f).read ()

# notename 0 == central C
distances = [0, 2, 4, 5, 7, 9, 11, 12]
def semitones (name, acc):
	return (name / 7 ) * 12 + distances[name % 7] + acc

# represent pitches as (notename, alteration), relative to C-major scale
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

def rat_to_lily_duration (rat):
	(n,d) = rat

	basedur = 1
	while d and  d % 2 == 0:
		basedur = basedur << 1
		d = d >> 1

	str = 's%d' % basedur
	if n <> 1:
		str = str + '*%d' % n
	if d <> 1:
		str = str + '/%d' % d

	return str

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


class Tuplet:
	def __init__ (self, number):
		self.start_note = number
		self.finale = []

	def append_finale (self, fin):
		self.finale.append (fin)

	def factor (self):
		n = self.finale[0][2]*self.finale[0][3]
		d = self.finale[0][0]*self.finale[0][1]
		return rat_simplify( (n, d))
	
	def dump_start (self):
		return '\\times %d/%d { ' % self.factor ()
	
	def dump_end (self):
		return ' }'

	def calculate (self, chords):
		edu_left = self.finale[0][0] * self.finale[0][1]

		startch = chords[self.start_note]
		c = startch
 		while c and edu_left:
			c.tuplet = self
			if c == startch:
				c.chord_prefix = self.dump_start () + c.chord_prefix 

			if not c.grace:
				edu_left = edu_left - c.EDU_duration ()
			if edu_left == 0:
				c.chord_suffix = c.chord_suffix+ self.dump_end ()
			c = c.next

		if edu_left:
			sys.stderr.write ("\nHuh? Tuplet starting at entry %d was too short." % self.start_note)
		
class Slur:
	def __init__ (self, number):
		self.number = number
		self.finale = []

	def append_entry (self, finale_e):
		self.finale.append (finale_e)

	def calculate (self, chords):
		startnote = self.finale[0][5]
		endnote = self.finale[3][2]
		try:
			cs = chords[startnote]
			ce = chords[endnote]

			if not cs or not ce:
				raise IndexError
			
			cs.note_suffix = '(' + cs.note_suffix 
			ce.note_prefix = ce.note_prefix + ')'
		except IndexError:
			sys.stderr.write ("""\nHuh? Slur no %d between (%d,%d), with %d notes""" % (self.number,  startnote, endnote, len (chords)))
					 
		
class Global_measure:
	def __init__ (self, number):
		self.timesig = ''
		self.number = number
		self.keysignature = None
		self.scale = None

		self.finale = []

	def __str__ (self):
		return `self.finale `
	
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


articulation_dict ={
	11: '\\prall',
	12: '\\mordent',
	8: '\\fermata',
	4: '^',
	1: '.',
	3: '>',
	18: '"arp"' , # arpeggio
};

class Articulation:
	def __init__ (self, a,b, finale):
		self.type = finale[0]
		self.notenumber = b
	def calculate (self, chords):
		c = chords[self.notenumber]

		try:
			a = articulation_dict[self.type]
		except KeyError:
			sys.stderr.write ("\nUnknown articulation no. %d on note no. %d" % (self.type, self.notenumber))
			sys.stderr.write ("\nPlease add an entry to articulation_dict in the Python source")
			a = '"art"'
			
		c.note_suffix = '-' + a + c.note_suffix

class Syllable:
	def __init__ (self, a,b , finale):
		self.chordnum = b
		self.syllable = finale[1]
		self.verse = finale[0]
	def calculate (self, chords, lyrics):
		self.chord = chords[self.chordnum]

class Verse:
	def __init__ (self, number, body):
		self.body = body
		self.number = number
		self.split_syllables ()
	def split_syllables (self):
		ss = re.split ('(-| +)', self.body)

		sep = 0
		syls = [None]
		for s in ss:
			if sep:
				septor = re.sub (" +", "", s)
				septor = re.sub ("-", " -- ", septor) 
				syls[-1] = syls[-1] + septor
			else:
				syls.append (s)
			
			sep = not sep 

		self.syllables = syls

	def dump (self):
		str = ''
		line = ''
		for s in self.syllables[1:]:
			line = line + ' ' + s
			if len (line) > 72:
				str = str + ' ' * 4 + line + '\n'
				line = ''
			
		str = """\nverse%s = \\lyrics {\n %s}\n""" %  (encodeint (self.number - 1) ,str)
		return str

	
class Measure:
	def __init__(self, no):
		self.number = no
		self.frames = [0] * 4
		self.flags = 0
		self.clef = 0
		self.finale = []
		self.global_measure = None
		self.staff = None
		self.valid = 1
		
	def add_finale_entry (self, entry):
		self.finale.append (entry)

	def valid (self):
		return self.valid
	def calculate (self):
		fs = []

		if len (self.finale) < 2:
			fs = self.finale[0]
			fs = map (string.atoi, list (fs))
			self.clef = fs[1]
			self.frames = [fs[0]]
		else:
			fs = self.finale[0] + self.finale[1]
			
			fs = map (string.atoi, list (fs))
			self.clef = fs[0]
			self.flags = fs[1]
			self.frames = fs[2:]


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

	def calculate (self):

		# do grace notes.
		lastch = None
		for c in self.chords:
			if c.grace and (lastch == None or (not lastch.grace)):
				c.chord_prefix = r'\grace {' + c.chord_prefix
			elif not c.grace and lastch and lastch.grace:
				lastch.chord_suffix = lastch.chord_suffix + ' } '

			lastch = c
			

		
	def dump (self):
		str = '%% FR(%d)\n' % self.number
		left = self.measure.global_measure.length ()

		
		ln = ''
		for c in self.chords:
			add = c.ly_string () + ' '
			if len (ln) + len(add) > 72:
				str = str + ln + '\n'
				ln = ''
			ln = ln + add
			left = rat_subtract (left, c.length ())

		str = str + ln 
		
		if left[0] < 0:
			sys.stderr.write ("""\nHuh? Going backwards in frame no %d, start/end (%d,%d)""" % (self.number, self.start, self.end))
			left = (0,1)
		if left[0]:
			str = str + rat_to_lily_duration (left)

		str = str + '  | \n'
		return str
		
def encodeint (i):
	return chr ( i  + ord ('A'))

class Staff:
	def __init__ (self, number):
		self.number = number
		self.measures = []

	def get_measure (self, no):
		if len (self.measures) <= no:
			self.measures = self.measures + [None]* (1 + no - len (self.measures))

		if self.measures[no] == None:
			m = Measure (no)
			self.measures [no] =m
			m.staff = self


		return self.measures[no]
	def staffid (self):
		return 'staff' + encodeint (self.number - 1)
	def layerid (self, l):
		return self.staffid() +  'layer%s' % chr (l -1 + ord ('A'))
	
	def dump_time_key_sigs (self):
		k  = ''
		last_key = None
		last_time = None
		last_clef = None
		gap = (0,1)
		for m in self.measures[1:]:
			if not m or not m.valid:
				continue # ugh.
			
			g = m.global_measure
			e = ''
			if g and last_key <> g.keysignature:
				e = e + "\\key %s \\major; " % lily_notename (g.keysignature)
				last_key = g.keysignature
			if g and last_time <> g.timesig :
				e = e + "\\time %d/%d; " % g.timesig
				last_time = g.timesig

			
			if last_clef <> m.clef :
				e = e + '\\clef "%s";' % lily_clef (m.clef)
				last_clef = m.clef
			if e:
				if gap <> (0,1):
					k = k +' ' + rat_to_lily_duration (gap) + '\n'
				gap = (0,1)
				k = k + e
				
			if g:
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
				if not m or not m.valid:
					sys.stderr.write ("Skipping non-existant measure")
					continue

				fr = None
				try:
					fr = m.frames[x]
				except IndexError:
					
					sys.stderr.write ("Skipping nonexistent frame")
					laystr = laystr + "% FOOBAR ! \n"
					print laystr
				if fr:
					first_frame = fr
					if gap <> (0,1):
						laystr = laystr +'} %s {\n ' % rat_to_lily_duration (gap)
						gap = (0,1)
					laystr = laystr + fr.dump ()
				else:
					if m.global_measure :
						gap = rat_add (gap, m.global_measure.length ())
					else:
						sys.stderr.write ( \
							"No global measure for staff %d measure %d\n"
							% (self.number, m.number))
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

				


class Chord:
	def __init__ (self, finale_entry):
		self.pitches = []
		self.frame = None
		self.finale = finale_entry
		self.duration  = None
		self.next = None
		self.prev = None
		self.note_prefix= ''
		self.note_suffix = ''
		self.chord_suffix = ''
		self.chord_prefix = ''
		self.tuplet = None
		self.grace = 0
		
	def measure (self):
		if not self.frame:
			return None
		return self.frame.measure

	def length (self):
		if self.grace:
			return (0,1)
		
		l = (1, self.duration[0])

		d = 1 << self.duration[1]

		dotfact = rat_subtract ((2,1), (1,d))
		mylen =  rat_multiply (dotfact, l)

		if self.tuplet:
			mylen = rat_multiply (mylen, self.tuplet.factor())
		return mylen
		
	def number (self):
		return self.finale[0][0]

	def EDU_duration (self):
		return self.finale[0][3]
	def set_duration (self):
		self.duration = EDU_to_duration(self.EDU_duration ())
	def calculate (self):
		self.find_realpitch ()
		self.set_duration ()

		flag = self.finale[0][5]
		if Chord.GRACE_MASK & flag:
			self.grace = 1
		
		
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
			self.chord_suffix = self.chord_suffix + ' ~ '
		
	REST_MASK = 0x40000000L
	TIE_START_MASK = 0x40000000L
	GRACE_MASK = 0x00800000L
	
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

		if not self.pitches:
			s  = 'r%d%s' % (self.duration[0] , '.'* self.duration[1])
		s = self.note_prefix + s + self.note_suffix
		if len (self.pitches) > 1:
			s = '<%s>' % s
		
		s = self.chord_prefix + s + self.chord_suffix
		return s

GFre = re.compile(r"""^\^GF\(([0-9-]+),([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
BCre = re.compile (r"""^\^BC\(([0-9-]+)\) ([0-9-]+) .*$""")
eEre = re.compile(r"""^\^eE\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) \$([0-9A-Fa-f]+) ([0-9-]+) ([0-9-]+)""")
FRre = re.compile (r"""^\^FR\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
MSre = re.compile (r"""^\^MS\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
note_re = re.compile (r"""^ +([0-9-]+) \$([A-Fa-f0-9]+)""")
Sxre  = re.compile (r"""^\^Sx\(([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
IMre = re.compile (r"""^\^IM\(([0-9-]+),([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
vere = re.compile(r"""^\^(ve|ch|se)\(([0-9-]+),([0-9-]+)\) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")
versere = re.compile(r"""^\^verse\(([0-9]+)\)(.*)\^end""")

TPre = re.compile(r"""^\^TP\(([0-9]+),([0-9]+)\) *([0-9-]+) ([0-9-]+) ([0-9-]+) ([0-9-]+)""")


class Etf_file:
	def __init__ (self, name):
		self.measures = [None]
		self.entries = [None]
		self.chords = [None]
		self.frames = [None]
		self.tuplets = [None]
		self.staffs = [None]
		self.slur_dict = {}
		self.articulations = [None]
		self.syllables = [None]
		self.verses = [None]
		
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
	def try_TP(self, l):
		m = TPre.match (l)
		if m:
			(nil, num) = map (string.atoi, (m.groups ()[0:2]))
			entries = map (string.atoi, (m.groups ()[2:]))

			if self.tuplets[-1] == None or num <> self.tuplets[-1].start_note:
				self.tuplets.append (Tuplet (num))

			self.tuplets[-1].append_finale (entries)
			
	def try_IM (self, l):
		m = IMre.match (l)
		if m:
			a = string.atoi (m.group (1))
			b = string.atoi (m.group (2))

			fin = map (string.atoi, m.groups ()[2:])

			self.articulations.append (Articulation (a,b,fin))
		return m
	def try_verse (self,l):
		m =  versere .match (l)
		if m:
			a = string.atoi (m.group (1))
			body =m.group (2)

			body = re.sub (r"""\^[a-z]+\([^)]+\)""", "", body)
			body = re.sub ("\^[a-z]+", "", body)
			self.verses.append (Verse (a, body))
			
		return m
	def try_ve (self,l):
		m = vere .match (l)
		if m:
			a = string.atoi (m.group (1))
			b = string.atoi (m.group (2))

			fin = map (string.atoi, m.groups ()[2:])

			self.syllables.append (Syllable (a,b,fin))
		return m
	def try_eE (self, l):
		m = eEre.match (l)
		if m:
			tup = m.groups()
			(no, prev, next, dur, pos, entryflag, extended, follow) = tup
			(no, prev, next, dur, pos,extended, follow) \
			  = tuple (map (string.atoi, [no,prev,next,dur,pos,extended,follow]))

			entryflag = string.atol (entryflag,16)
			if len (self.entries) <= no:
				# missing entries seem to be quite common.
				# we fill'em up with None.
				self.entries = self.entries + [None] * (no - len (self.entries) + 1)
					
			current_entry = ((no, prev, next, dur, pos, entryflag, extended, follow), [])
			self.entries[no] = current_entry
		return m

	def try_Sx(self,l):
		m = Sxre.match (l)
		if m:
			slurno = string.atoi (m.group (1))

			sl = None
			try:
				sl = self.slur_dict[slurno]
			except KeyError:
				sl = Slur (slurno)
				self.slur_dict[slurno] = sl

			params = list (m.groups ()[1:])
			params = map (string.atoi, params)
			sl.append_entry (params)

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
			if len (self.frames) <= frameno:
				self.frames = self.frames + [None]  * (frameno - len(self.frames) + 1)
			
			self.frames[frameno] = Frame ((frameno, startnote, endnote))
			
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

		gulp = open (name).read ()

		gulp = re.sub ('[\n\r]+', '\n',  gulp)
		ls = string.split (gulp, '\n')
		
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
				m = self.try_IM (l)
			if not m:
				m = self.try_Sx (l)
			if not m:
				m = self.try_TP (l)
			if not m:
				m = self.try_verse (l)

		sys.stderr.write ('processing ...')
		sys.stderr.flush ()

		self.unthread_entries ()

		for st in self.staffs[1:]:
			if not st:
				continue
			mno = 1
			for m in st.measures[1:]:
				if not m:
					continue
				
				m.calculate()
				try:
					m.global_measure = self.measures[mno]
				except IndexError:
					sys.stderr.write ("Non-existent global measure %d" % mno)
					continue
				
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
			if c:
				c.calculate()

		for f in self.frames[1:]:
			if f:
				f.calculate ()
			
		for t in self.tuplets[1:]:
			t.calculate (self.chords)
			
		for s in self.slur_dict.values():
			s.calculate (self.chords)
		for s in self.articulations[1:]:
			s.calculate (self.chords)
			
	def get_thread (self, startno, endno):

		thread = []

		c = None
		try:
			c = self.chords[startno]
		except IndexError:
			sys.stderr.write ("Huh? Frame has invalid bounds (%d,%d)\n" % (startno, endno))
			return []

		
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


		# should use \addlyrics ?

		for v in self.verses[1:]:
			str = str + v.dump()

		if len (self.verses) > 1:
			sys.stderr.write ("\nLyrics found; edit to use \\addlyrics to couple to a staff\n")
			
		if staffs:
			str = str + '\\score { < %s > } ' % string.join (staffs)
			
		return str


	def __str__ (self):
		return 'ETF FILE %s %s' % (self.measures,  self.entries)
	
	def unthread_entries (self):
		self.chords = [None]
		for e in self.entries[1:]:
			ch = None
			if e:		
				ch = Chord (e)
			self.chords.append (ch)
				
		for e in self.chords[1:]:
			if not e:
				continue
			e.prev = self.chords[e.finale[0][1]]
			e.next = self.chords[e.finale[0][2]]

def identify():
	sys.stderr.write ("%s from LilyPond %s\n" % (program_name, version))

def help ():
	print """Usage: etf2ly [OPTION]... ETF-FILE

Convert ETF to LilyPond.

Options:
  -h,--help          this help
  -o,--output=FILE   set output filename to FILE
  -v,--version       version information

Enigma Transport Format is a format used by Coda Music Technology's
Finale product. This program will convert a subset of ETF to a
ready-to-use lilypond file.

Report bugs to bug-gnu-music@gnu.org

Written by  Han-Wen Nienhuys <hanwen@cs.uu.nl>
"""

def print_version ():
	print r"""etf2ly (GNU lilypond) %s

This is free software.  It is covered by the GNU General Public License,
and you are welcome to change it and/or distribute copies of it under
certain conditions.  Invoke as `midi2ly --warranty' for more information.

Copyright (c) 2000 by Han-Wen Nienhuys <hanwen@cs.uu.nl>
""" % version



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

e = None
for f in files:
	if f == '-':
		f = ''

	sys.stderr.write ('Processing `%s\'\n' % f)
	e = Etf_file(f)
	if not out_filename:
		out_filename = os.path.basename (re.sub ('(?i).etf$', '.ly', f))
		
	if out_filename == f:
		out_filename = os.path.basename (f + '.ly')
		
	sys.stderr.write ('Writing `%s\'' % out_filename)
	ly = e.dump()

	
	
	fo = open (out_filename, 'w')
	fo.write ('%% lily was here -- automatically converted by etf2ly from %s\n' % f)
	fo.write(ly)
	fo.close ()
	
