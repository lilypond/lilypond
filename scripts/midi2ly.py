#!@PYTHON@

import midi
import sys
import string

LINE_BELL = 60
scale_steps = [0,2,4,5,7,9,11]

def split_track (track):
	chs = {}
	for i in range(16):
		chs[i] = []
		
	for e in track:
		data = list (e[1])
		if data[0] > 0x7f and data[0] < 0xf0:
			c = data[0] & 0x0f
			e = (e[0], tuple ([data[0] & 0xf0] + data[1:]))
			chs[c].append (e)
		else:
			chs[0].append (e)

	for i in range (16):
		if chs[i] == []:
			del chs[i]

	whatarewes = []
	for v in chs.values ():
		ns = notes_on_channel (v)
		dinges = unthread_notes (ns)
		if len (dinges):
			whatarewes.append (dinges)
	return whatarewes


class Note:
	def __init__ (self, duration, pitch, velocity):
		self.velocity = velocity
		self.pitch = pitch
		self.duration = duration

	def duration_compare (a, b):
		if a.duration < b.duration:
			return -1
		elif a.duration > b.duration:
			return 1
		else:
			return 0

	def dump (self):
		return dump_note (self)

class Time:
	def __init__ (self, t, num, den):
		self.duration = t
		self.num = num
		self.den = den
		
	def dump (self):
		return dump_skip (self.duration) + '\\time %d/%d ' % (self.num, self.den)

class Key:
	key_sharps = ('c', 'g', 'd', 'a', 'e', 'b', 'fis')
	key_flats = ('BUG', 'f', 'bes', 'es', 'as', 'des', 'ges')

	def __init__ (self, t, sharps, flats, minor):
		self.duration = t
		self.flats = flats
		self.sharps = sharps
		self.minor = minor
		
	def dump (self):
		if self.sharps and self.flats:
			s = '\\keysignature %s ' % 'TODO'
		elif self.sharps:
			s = '\\notes\\key %s \major' % key_sharps[self.sharps]
		elif self.flats:
			s = '\\notes\\key %s \major' % key_flats[self.flats]
		return dump_skip (self.duration) + s


class Text:
	def __init__ (self, text):
		self.text = text
		self.duration = 0
		
	def dump (self):
		return dump_text (self)

def notes_on_channel (channel):
	pitches = {}

	nch = []
	for e in channel:
		t = e[0]

		if e[1][0] == midi.NOTE_ON:
			if not pitches.has_key (e[1][1]):
				pitches[e[1][1]] = (t, e[1][2])
		elif e[1][0] == midi.NOTE_OFF:
			try:
				(lt, vel) = pitches[e[1][1]]
				del pitches[e[1][1]]
				
				nch.append ((t, Note (t-lt, e[1][1], vel)))
				
			except KeyError:
				pass
		elif e[1][0] == midi.META_EVENT:
			if e[1][1] == midi.TIME_SIGNATURE:
				(num, den, clocks4, count32) = map (ord, e[1][2])
				nch.append ((t, Time (t, num, den)))
			elif e[1][1] == midi.KEY_SIGNATURE:
				(accidentals, minor) = map (ord, e[1][2])
				sharps = 0
				flats = 0
				if accidentals < 127:
					sharps = accidentals
				else:
					flats = 256 - accidentals
				nch.append ((t, Key (t, sharps, flats, minor)))
			elif e[1][1] == midi.TEXT_EVENT:
				nch.append ((t, Text (e[1][2])))
			else:
				sys.stderr.write ("SKIP: %s\n" % `e`)
				pass
		else:
			sys.stderr.write ("SKIP: %s\n" % `e`)
			pass
	
	return nch

def unthread_notes (channel):
	threads = []
	while channel:
		thread = []
		end_busy_t = 0
		start_busy_t = 0
		todo = []
		for e in channel:
			t = e[0]
			if e[1].__class__ == Note and ((t == start_busy_t and e[1].duration + t == end_busy_t) \
			    or t >= end_busy_t):
				thread.append (e)
				start_busy_t = t
				end_busy_t = t + e[1].duration
			elif e[1].__class__ == Time or e[1].__class__ == Key or e[1].__class__ == Text:
				thread.append (e)
			else:
				todo.append (e)
		threads.append (thread)
		channel = todo

	return threads

def gcd (a,b):
	if b == 0:
		return a
	c = a
	while c: 
		c = a % b
		a = b
		b = c
	return a
	
def dump_skip (dt):
	return 's' + dump_duration (dt)

def dump_duration (dur):
	g = gcd (dur, 384)
	s = '4'
	(p,q) = (dur / g, 384 / g)
	if (p == 1 and q == 1) :
		pass
	else:
		if p <> 1:	
			s = s + '*%d'% p
		if q <> 1:
			s = s + '*%d'% q
	return s
	
def dump_note (note):
	p = note.pitch
	oct = p / 12
	step = p % 12

	i = 0
	while  i < len (scale_steps):
		if scale_steps[i] > step:
			break
		i = i+1
		
	i = i-1
	str = chr ((i + 2)  % 7 + ord ('a'))
	if scale_steps[i] <> step:
		str = str + 'is'

	return ' %s' % str + dump_duration (note.duration)

def dump (self):
	return self.dump ()

def dump_text (text):
	return '\n  % ' + text.text + '\n  '

def dump_chord (ch):
	s = ''
	if len(ch) == 1:
		s = dump (ch[0])
	else:
		s = '<' + string.join (map (dump, ch)) + '>'
	return s

# thread?
def dump_channel (thread):
	last_e = None
	chs = []
	ch = []

	for e in thread:
		if last_e and last_e[0] == e[0]:
			ch.append (e[1])
		else:
			if ch:
				chs.append ((last_e[0], ch))
				
			ch = [e[1]]
			
		last_e = e

	if ch:
		chs.append ((last_e[0], ch))
	t = 0
	last_t = 0

	lines = ['']
	for ch in chs:
		if len (lines[-1]) > LINE_BELL:
			lines.append ('')
			
		t = ch[0]
		if t - last_t:
			lines[-1] = lines[-1] + dump_skip (t-last_t)
			
		lines[-1] = lines[-1] + dump_chord (ch[1])

		last_t = t + ch[1][0].duration

	return string.join (lines, '\n  ') + '\n'

def dump_notes (channel):
	on_hold = []
	s = ''
	for e in channel:
		if e[0] <> last_t:
			s = s + dump_chord (on_hold)
			on_hold = []
			last_t = e[0]

		on_hold.append (e)
	return s

def track_name (i):
	return 'track%c' % (i + ord ('A'))

def channel_name (i):
	return 'channel%c' % (i + ord ('A'))

def dump_track (channels, n):
	s = '\n'
	track = track_name (n)
	for i in range (len (channels)):
		channel = channel_name (i)
		s = s + '%s = \\notes {\n' % (track + channel)
		s = s + '  ' + dump_channel (channels[i][0])
		s = s + '}\n\n'

	s = s + '%s = <\n' % track

	for i in range (len (channels)):
		channel = channel_name (i)
		s = s + '  \\context Voice = %s \\%s\n' % (channel, track + channel)
	s = s + '>\n\n'
	return s
			
	
def convert_midi (f):
	str = open (f).read ()
	midi_dump = midi.parse (str)

	tracks = []
	for t in midi_dump[1]:
		tracks.append (split_track (t))

	s = ''
	for i in range (len (tracks)):
		s = s + dump_track (tracks[i], i)

	s = s + '\n\\score {\n  <\n'
	for i in range (len (tracks)):
		track = track_name (i)
		s = s + '    \\context Staff=%s \\%s\n' % (track, track)
	s = s + '  >\n}\n'
	
	sys.stdout.write (s)
		

for f in sys.argv[1:]:
	convert_midi (f)
