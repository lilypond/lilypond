#!@PYTHON@
#
# midi2ly.py -- LilyPond midi import script
# 
# source file of the GNU LilyPond music typesetter
#
# convert MIDI to LilyPond source
#


'''
TODO:

   * fix duration bug for lily input: a4*3/5
   * handle all notes off
   * recognise (note-on, volume = 0) as note-off
   * don't ever quant skips
   * handle lyrics and other text events

'''

import getopt
import __main__
import sys
import string

sys.path.append ('@datadir@/python')
sys.path.append ('@datadir@/buildscripts/out')
sys.path.append ('@datadir@/modules/out')

import midi

try:
	import gettext
	gettext.bindtextdomain ('lilypond', '@localedir@')
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

# Attempt to fix problems with limited stack size set by Python!
# Sets unlimited stack size. Note that the resource module only
# is available on UNIX.
try:
       import resource
       resource.setrlimit (resource.RLIMIT_STACK, (-1, -1))
except:
       pass

program_name = 'midi2ly [experimental]'
package_name = 'lilypond'
help_summary = _ ("Convert MIDI to LilyPond source")

option_definitions = [
	('', 'h', 'help', _ ("this help")),
	(_ ("DUR"), 'd', 'duration-quant', _ ("quantise note durations on DUR")),
	(_ ("ACC[:MINOR]"), 'k', 'key', _ ("set key: ACC=+sharps|-flats; MINOR=1")),
	(_ ("FILE"), 'o', 'output', _ ("write ouput to FILE")),
	(_ ("DUR"), 's', 'start-quant', _ ("quantise note starts on DUR")),
	(_ ("DUR*NUM/DEN"), 't', 'allow-tuplet', _ ("allow tuplet durations DUR*NUM/DEN")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	]

from lilylib import *




class Note:
	names = (0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6)
	accidentals = (0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0)
	acc_name = ('eses', 'es', 'BUG', 'is' , 'isis')
	
	def __init__ (self, clocks, pitch, velocity):
		self.velocity = velocity
		self.pitch = pitch
		self.clocks = clocks

	def clocks_compare (a, b):
		if a.clocks < b.clocks:
			return -1
		elif a.clocks > b.clocks:
			return 1
		else:
			return 0

	def dump (self):
		# major scale: do-do
		# minor scale: la-la  (= + 5) '''

		n = self.names[(self.pitch) % 12]
 		a = self.accidentals[(self.pitch) % 12]

		if a and key.flats:
			a = - self.accidentals[(self.pitch) % 12]
			n = (n - a) % 7

		name = chr ((n + 2)  % 7 + ord ('a'))

		if a:
			name = name + self.acc_name[a + 2]

		#  By tradition, all scales now consist of a sequence
		#  of 7 notes each with a distinct name, from amongst
		#  a b c d e f g.  But, minor scales have a wide
		#  second interval at the top - the 'leading note' is
		#  sharped. (Why? it just works that way! Anything
		#  else doesn't sound as good and isn't as flexible at
		#  saying things. In medieval times, scales only had 6
		#  notes to avoid this problem - the hexachords.)

		#  So, the d minor scale is d e f g a b-flat c-sharp d
		#  - using d-flat for the leading note would skip the
		#  name c and duplicate the name d.  Why isn't c-sharp
		#  put in the key signature? Tradition. (It's also
		#  supposedly based on the Pythagorean theory of the
		#  cycle of fifths, but that really only applies to
		#  major scales...)  Anyway, g minor is g a b-flat c d
		#  e-flat f-sharp g, and all the other flat minor keys
		#  end up with a natural leading note. And there you
		#  have it.

		#  John Sankey <bf250@freenet.carleton.ca>
		#
		#  Let's also do a-minor: a b c d e f gis a
		#
		#  --jcn

		o = self.pitch / 12 - 4

		if key.minor:
			if key.sharps == 0 and key.flats == 0 and name == 'as':
				name = 'gis'
       			elif key.flats == 1 and name == 'des':
				name = 'cis'
			elif key.flats == 2 and name == 'ges':
				name = 'fis'
			elif key.sharps == 5 and name == 'g':
				name = 'fisis'
			elif key.sharps == 6 and name == 'd':
				name = 'cisis'
			elif key.sharps == 7 and name == 'a':
				name = 'gisis'

		if key.flats >= 6 and name == 'b':
 			name = 'ces'
			o = o + 1
		if key.flats >= 7 and name == 'e':
 			name = 'fes'

		if key.sharps >= 3 and name == 'f':
 			name = 'eis'
		if key.sharps >= 4 and name == 'c':
 			name = 'bis'
			o = o - 1

                s = name
		
		if o > 0:
			s = s + "'" * o
		elif o < 0:
			s = s + "," * -o

		return s + dump_duration (self.clocks) + ' '


class Time:
	def __init__ (self, num, den):
		self.clocks = 0
		self.num = num
		self.den = den

	def dump (self):
		return '\n  ' + '\\time %d/%d ' % (self.num, self.den) + '\n  '

class Tempo:
	def __init__ (self, seconds_per_1):
		self.clocks = 0
		self.seconds_per_1 = seconds_per_1

	def dump (self):
		# return '\n  ' + '\\tempo 1 = %d ' % (60 / self.seconds_per_1) + '\n  '
		return '\n  ' + '\\tempo 4 = %d ' % (4 * 60 / self.seconds_per_1) + '\n  '

class Key:
	key_sharps = ('c', 'g', 'd', 'a', 'e', 'b', 'fis')
	key_flats = ('BUG', 'f', 'bes', 'es', 'as', 'des', 'ges')

	def __init__ (self, sharps, flats, minor):
		self.clocks = 0
		self.flats = flats
		self.sharps = sharps
		self.minor = minor

	def dump (self):
		global key
		key = self

		s = ''
		if self.sharps and self.flats:
			s = '\\keysignature %s ' % 'TODO'
		else:
			
			if self.flats:
				k = (ord ('cfbeadg'[self.flats % 7]) - ord ('a') - 2 -2 * self.minor + 7) % 7
			else:
				k = (ord ('cgdaebf'[self.sharps % 7]) - ord ('a') - 2 -2 * self.minor + 7) % 7
  
			if not self.minor:
				name = chr ((k + 2) % 7 + ord ('a'))
			else:
				name = chr ((k + 2) % 7 + ord ('a'))

			# fis cis gis dis ais eis bis
			sharps = (2, 4, 6, 1, 3, 5, 7)
			# bes es as des ges ces fes
			flats = (6, 4, 2, 7, 5, 3, 1)
			a = 0
			if self.flats:
				if flats[k] <= self.flats:
					a = -1
			else:
				if sharps[k] <= self.sharps:
					a = 1

			if a:
				name = name + Note.acc_name[a + 2]

			s = '\\key ' + name
			if self.minor:
				s = s + ' \\minor'
			else:
				s = s + ' \\major'

		return '\n\n  ' + s + '\n  '


# TODO: really handle lyrics
class Text:
	text_types = (
		'SEQUENCE_NUMBER',
		'TEXT_EVENT',
		'COPYRIGHT_NOTICE',
		'SEQUENCE_TRACK_NAME',
		'INSTRUMENT_NAME',
		'LYRIC',
		'MARKER',
		'CUE_POINT',)
	
	def __init__ (self, type, text):
		self.clocks = 0
		self.type = type
		self.text = text

	def dump (self):
		return '\n  % [' + self.text_types[self.type] + '] ' + self.text + '\n  '


output_name = ''
LINE_BELL = 60
scale_steps = [0,2,4,5,7,9,11]

clocks_per_1 = 1536
key = 0
start_quant = 0
start_quant_clocks = 0
duration_quant = 0
duration_quant_clocks = 0
allowed_tuplets = []
allowed_tuplet_clocks = []

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

	threads = []
	for v in chs.values ():
		events = events_on_channel (v)
		thread = unthread_notes (events)
		if len (thread):
			threads.append (thread)
	return threads


def quantise_clocks (clocks, quant):
 	q = int (clocks / quant) * quant
	if q != clocks:
		for tquant in allowed_tuplet_clocks:
		 	if int (clocks / tquant) * tquant == clocks:
				return clocks
	 	if 2 * (clocks - q) > quant:
 			q = q + quant
	return q

def events_on_channel (channel):
	pitches = {}

	notes = []
	events = []
	for e in channel:
		t = e[0]

		if start_quant_clocks:
			t = quantise_clocks (t, start_quant_clocks)


		if e[1][0] == midi.NOTE_ON:
			if not pitches.has_key (e[1][1]):
				pitches[e[1][1]] = (t, e[1][2])
		elif e[1][0] == midi.NOTE_OFF:
			try:
				(lt, vel) = pitches[e[1][1]]
				del pitches[e[1][1]]

				i = len (notes) - 1 
				while i > 0:
					if notes[i][0] > lt:
						i = i -1
					else:
						break
				d = t - lt
				if duration_quant_clocks:
					d = quantise_clocks (d, duration_quant_clocks)
					if not d:
						d = duration_quant_clocks

				notes.insert (i + 1,
					    (lt, Note (d, e[1][1], vel)))

			except KeyError:
				pass
		elif e[1][0] == midi.META_EVENT:
			if e[1][1] == midi.SET_TEMPO:
				(u0, u1, u2) = map (ord, e[1][2])
				us_per_4 = u2 + 256 * (u1 + 256 * u0)
				seconds_per_1 = us_per_4 * 4 / 1e6
				events.append ((t, Tempo (seconds_per_1)))
			elif e[1][1] == midi.TIME_SIGNATURE:
				(num, dur, clocks4, count32) = map (ord, e[1][2])
				den = 2 ** dur 
				events.append ((t, Time (num, den)))
			elif e[1][1] == midi.KEY_SIGNATURE:
 				(accidentals, minor) = map (ord, e[1][2])
				sharps = 0
				flats = 0
				if accidentals < 127:
					sharps = accidentals
				else:
					flats = 256 - accidentals

				events.append ((t, Key (sharps, flats, minor)))
			elif e[1][1] >= midi.SEQUENCE_NUMBER and e[1][1] <= midi.CUE_POINT:
				events.append ((t, Text (e[1][1], e[1][2])))
			else:
				sys.stderr.write ("SKIP: %s\n" % `e`)
				pass
		else:
			sys.stderr.write ("SKIP: %s\n" % `e`)
			pass

	i = 0
	while len (notes):
		if i < len (events) and notes[0][0] >= events[i][0]:
			i = i + 1
		else:
			events.insert (i, notes[0])
			del notes[0]
	return events

def unthread_notes (channel):
	threads = []
	while channel:
		thread = []
		end_busy_t = 0
		start_busy_t = 0
		todo = []
		for e in channel:
			t = e[0]
			if e[1].__class__ == Note and ((t == start_busy_t and e[1].clocks + t == end_busy_t) \
			    or t >= end_busy_t):
				thread.append (e)
				start_busy_t = t
				end_busy_t = t + e[1].clocks
			elif e[1].__class__ == Time or e[1].__class__ == Key or e[1].__class__ == Text or e[1].__class__ == Tempo:
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
	
def dump_skip (clocks):
	return 's' + dump_duration (clocks) + ' '

def dump_duration (clocks):
	for i in range (len (allowed_tuplet_clocks)):
		if clocks == allowed_tuplet_clocks[i]:
 			(dur, num, den) = allowed_tuplets[i]
			s = '%d*%d/%d' % (dur, num, den)
			return s

	g = gcd (clocks, clocks_per_1)
	if g:
		(d, n) = (clocks_per_1 / g, clocks / g)
		if n == 1:
			s = '%d' % d
		elif n == 3 and d != 1:
			s = '%d.' % (d / 2)
		else:
			s = '%d*%d' % (d, n)
	else:
		s = '1*%d/%d' % (clocks, clocks_per_1)
	return s
	
def dump (self):
	return self.dump ()

def dump_chord (ch):
	s = ''
	notes = []
	for i in ch:
		if i.__class__ == Note:
			notes.append (i)
		else:
			s = s + i.dump ()
	if len (notes) == 1:
		s = s + dump (notes[0])
	elif len (notes) > 1:
		s = s + '<' + string.join (map (dump, notes)) + '>'
	return s

# thread?
def dump_channel (thread):
	global key

	key = Key (0, 0, 0)
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
		i = string.rfind (lines[-1], '\n')
		if len (lines[-1][i:]) > LINE_BELL:
			lines.append ('')
			
		t = ch[0]
		if t - last_t:
			lines[-1] = lines[-1] + dump_skip (t-last_t)
			
		lines[-1] = lines[-1] + dump_chord (ch[1])

		clocks = 0
		for i in ch[1]:
			if i.clocks > clocks:
				clocks = i.clocks
				
		last_t = t + clocks

	return string.join (lines, '\n  ') + '\n'

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
			
def convert_midi (f, o):
	global clocks_per_1

	str = open (f).read ()
	midi_dump = midi.parse (str)

	clocks_per_1 = midi_dump[0][1]

	global start_quant, start_quant_clocks
	if start_quant:
		start_quant_clocks = clocks_per_1 / start_quant

	global duration_quant, duration_quant_clocks
	if duration_quant:
		duration_quant_clocks = clocks_per_1 / duration_quant

	global allowed_tuplet_clocks
	allowed_tuplet_clocks = []
	for (dur, num, den) in allowed_tuplets:
		allowed_tuplet_clocks.append (clocks_per_1 * num / (dur * den))

	tracks = []
	for t in midi_dump[1]:
		tracks.append (split_track (t))

	tag = '%% Lily was here -- automatically converted by %s from %s' % ( program_name, f)

	s = ''
	s = tag + '\n\n'
	for i in range (len (tracks)):
		s = s + dump_track (tracks[i], i)

	s = s + '\n\\score {\n  <\n'
	for i in range (len (tracks)):
		track = track_name (i)
		s = s + '    \\context Staff=%s \\%s\n' % (track, track)
	s = s + '  >\n}\n'

 	progress (_ ("%s output to `%s'...") % ('LY', o))

	if o == '-':
		h = sys.stdout
	else:
		h = open (o, 'w')

	h.write (s)
	h.close ()


(sh, long) = getopt_args (__main__.option_definitions)
try:
	(options, files) = getopt.getopt(sys.argv[1:], sh, long)
except getopt.error, s:
	errorport.write ('\n')
	errorport.write (_ ("error: ") + _ ("getopt says: `%s\'" % s))
	errorport.write ('\n')
	errorport.write ('\n')
	help ()
	sys.exit (2)
	
for opt in options:	
	o = opt[0]
	a = opt[1]

	if 0:
		pass
	elif o == '--help' or o == '-h':
		help ()
		errorport.write ('\n')
		errorport.write (_ ("Example:"))
		errorport.write  (r'''
    midi2ly --key=-2:1 --duration-quant=32 \
        --allow-tuplet=4*2/3 --allow-tuplet=2*4/3 foo.midi
''')
		sys.exit (0)
	elif o == '--output' or o == '-o':
		output_name = a
	elif o == '--verbose' or o == '-V':
		verbose_p = 1
	elif o == '--version' or o == '-v':
		identify ()
		sys.exit (0)
	elif o == '--warranty' or o == '-w':
		status = system ('lilypond -w', ignore_error = 1)
		if status:
			warranty ()
		sys.exit (0)
	elif o == '--key' or o == '-k':
		(accidentals, minor) = map (string.atoi, string.split (a + ':0', ':'))[0:2]
 		sharps = 0
 		flats = 0
 		if accidentals >= 0:
 			sharps = accidentals
 		else:
 			flats = - accidentals
		global key
		key = Key (sharps, flats, minor)
	elif o == '--duration-quant' or o == '-d':
		global duration_quant
		duration_quant = string.atoi (a)
	elif o == '--start-quant' or o == '-s':
		global start_quant, start_quant_clocks
		start_quant = string.atoi (a)
	elif o == '--allow-tuplet' or o == '-t':
		global allowed_tuplets
		a = string.replace (a, '/', '*')
		tuplet = map (string.atoi, string.split (a, '*'))
		allowed_tuplets.append (tuplet)


if not files or files[0] == '-':

	# FIXME: read from stdin when files[0] = '-'
	help ()
	errorport.write (program_name + ":" + _ ("error: ") + _ ("no files specified on command line.") + '\n')
	sys.exit (2)


for f in files:

	g = f
	g = strip_extension (g, '.midi')
	g = strip_extension (g, '.mid')
	g = strip_extension (g, '.MID')
	(outdir, outbase) = ('','')

	if not output_name:
		outdir = '.'
		outbase = os.path.basename (g)
		o = os.path.join (outdir, outbase + '-midi.ly')
	elif output_name[-1] == os.sep:
		outdir = output_name
		outbase = os.path.basename (g)
		os.path.join (outdir, outbase + '-gen.ly')
	else:
		o = output_name
 		(outdir, outbase) = os.path.split (o)

	if outdir != '.':
		mkdir_p (outdir, 0777)

	convert_midi (f, o)

