#!@PYTHON@

import midi
import sys
import string

scale_steps = [0,2,4,5,7,9,11]

def split_track (track):
	chs = {}
	for i in range(16):
		chs[i] = []
		
	for e in track:
		data = list (e[1])		
		c = data[0] & 0x0f
		e = (e[0], tuple ([data[0] & 0xf0] + data[1:]))
		chs[c].append (e)

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

	def duration_compare (a,b):
		if a.duration < b.duration :
			return -1
		elif a.duration > b.duration:
			return 1
		else:
			return 0
	
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
		else:
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
			if (t == start_busy_t and e[1].duration + t == end_busy_t) \
			    or t >= end_busy_t:
				thread.append (e)
				start_busy_t = t
				end_busy_t = t + e[1].duration
			else:
				todo.append(e)
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
			s = '*%d'% p
		if q <> 1:
			s = '*%d'% q
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

def dump_chord (ch):
	s = ''
	if len(ch) == 1:
		s = dump_note (ch[0])
	else:
		s = '<' + string.join (map (dump_note, ch)) + '>'
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

	s = ''
	for ch in chs:
		t = ch[0]
		if t - last_t:
			s = s + dump_skip (t-last_t)
			
		s = s + dump_chord (ch[1])
		last_t = t + ch[1][0].duration
		
	return s

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

def dump_track (channels, n):
	s = ''
	track = 'track%c' % (n + ord ('A'))
	for i in range (len (channels)):
		channel = 'channel%c' % (i + ord ('A'))
		s = s + '%s = \\context Thread=%s \\notes {\n' % (track + channel, track + channel)
		s = s + '  ' + dump_channel (channels[i][0])
		s = s + '\n}\n'

	s = s + '%s = \\context Staff=%s <\n' % (track, track)

	for i in range (len (channels)):
		channel = 'channel%c' % (i + ord ('A'))
		s = s + '  \\context Voice = %s \\%s\n' % (channel, track + channel)
	s = s + '\n>\n'
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
		track = 'track%c' % (i + ord ('A'))
		s = s + '    \\context Staff=%s \\%s\n' % (track, track)
	s = s + '\n  >\n}\n'
	
	sys.stdout.write (s)
		

for f in sys.argv[1:]:
	convert_midi (f)
