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
   * test on weird and unquantised midi input (lily-devel)
   * drop c++ midi2ly
   * update doc and manpage

   * simply insert clef changes whenever too many ledger lines
     [to avoid tex capacity exceeded]
   * do not ever quant skips
   * better lyrics handling
   * [see if it is feasible to] move ly-classes to library for use in
     other converters, while leaving midi specific stuff here
'''

import os
import sys
import getopt
import sys
import string


# do fuddling: we must load the midi module from the right directory. 
datadir = '@datadir@'
if os.environ.has_key ('LILYPONDPREFIX'):
	datadir = os.environ['LILYPONDPREFIX']
else:
	datadir = '@datadir@'

sys.path.append (os.path.join (datadir, 'python'))
sys.path.append (os.path.join (datadir, 'python/out'))

import midi

################################################################
################ CONSTANTS


output_name = ''
LINE_BELL = 60
scale_steps = [0,2,4,5,7,9,11]

clocks_per_1 = 1536
clocks_per_4 = 0
key = 0
time = 0
reference_note = 0
start_quant = 0
start_quant_clocks = 0
duration_quant = 0
duration_quant_clocks = 0
allowed_tuplets = []
allowed_tuplet_clocks = []
absolute_p = 0
explicit_durations_p = 0
text_lyrics_p = 0



################################################################

localedir = '@localedir@'
try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

program_name = 'midi2ly'
program_version = '@TOPLEVEL_VERSION@'

errorport = sys.stderr
verbose_p = 0

# temp_dir = os.path.join (original_dir,  '%s.dir' % program_name)
# original_dir = os.getcwd ()
# keep_temp_dir_p = 0


help_summary = _ ("Convert MIDI to LilyPond source")

option_definitions = [
	('', 'a', 'absolute-pitches', _ ("print absolute pitches")),
	(_ ("DUR"), 'd', 'duration-quant', _ ("quantise note durations on DUR")),
	('', 'e', 'explicit-durations', _ ("print explicit durations")),
	('', 'h', 'help', _ ("this help")),
	(_ ("ALT[:MINOR]"), 'k', 'key', _ ("set key: ALT=+sharps|-flats; MINOR=1")),
	(_ ("FILE"), 'o', 'output', _ ("write ouput to FILE")),
	(_ ("DUR"), 's', 'start-quant', _ ("quantise note starts on DUR")),
	(_ ("DUR*NUM/DEN"), 't', 'allow-tuplet', _ ("allow tuplet durations DUR*NUM/DEN")),
	('', 'V', 'verbose', _ ("verbose")),
	('', 'v', 'version', _ ("print version number")),
	('', 'w', 'warranty', _ ("show warranty and copyright")),
	('', 'x', 'text-lyrics', _ ("treat every text as a lyric")),
	]

################################################################
# lilylib.py -- options and stuff
# 
# source file of the GNU LilyPond music typesetter

import os

try:
	import gettext
	gettext.bindtextdomain ('lilypond', localedir)
	gettext.textdomain ('lilypond')
	_ = gettext.gettext
except:
	def _ (s):
		return s

if program_version == '@' + 'TOPLEVEL_VERSION' + '@':
	program_version = '1.5.17'

def identify ():
	sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
	identify ()
	sys.stdout.write ('\n')
	sys.stdout.write (_ ('Copyright (c) %s by' % ' 2001--2002'))
	sys.stdout.write ('\n')
	sys.stdout.write ('  Han-Wen Nienhuys')
	sys.stdout.write ('  Jan Nieuwenhuizen')
	sys.stdout.write ('\n')
	sys.stdout.write (_ (r'''
Distributed under terms of the GNU General Public License. It comes with
NO WARRANTY.'''))
	sys.stdout.write ('\n')

def progress (s):
	errorport.write (s + '\n')

def warning (s):
	progress (_ ("warning: ") + s)
		
def error (s):


	'''Report the error S.  Exit by raising an exception. Please
	do not abuse by trying to catch this error. If you do not want
	a stack trace, write to the output directly.

	RETURN VALUE

	None
	
	'''
	
	progress (_ ("error: ") + s)
	raise _ ("Exiting ... ")

def getopt_args (opts):
	'''Construct arguments (LONG, SHORT) for getopt from  list of options.'''
	short = ''
	long = []
	for o in opts:
		if o[1]:
			short = short + o[1]
			if o[0]:
				short = short + ':'
		if o[2]:
			l = o[2]
			if o[0]:
				l = l + '='
			long.append (l)
	return (short, long)

def option_help_str (o):
	'''Transform one option description (4-tuple ) into neatly formatted string'''
	sh = '  '	
	if o[1]:
		sh = '-%s' % o[1]

	sep = ' '
	if o[1] and o[2]:
		sep = ','
		
	long = ''
	if o[2]:
		long= '--%s' % o[2]

	arg = ''
	if o[0]:
		if o[2]:
			arg = '='
		arg = arg + o[0]
	return '  ' + sh + sep + long + arg


def options_help_str (opts):
	'''Convert a list of options into a neatly formatted string'''
	w = 0
	strs =[]
	helps = []

	for o in opts:
		s = option_help_str (o)
		strs.append ((s, o[3]))
		if len (s) > w:
			w = len (s)

	str = ''
	for s in strs:
		str = str + '%s%s%s\n' % (s[0], ' ' * (w - len(s[0])  + 3), s[1])
	return str

def help ():
	ls = [(_ ("Usage: %s [OPTION]... FILE") % program_name),
		('\n\n'),
		(help_summary),
		('\n\n'),
		(_ ("Options:")),
		('\n'),
		(options_help_str (option_definitions)),
		('\n\n'),
		(_ ("Report bugs to %s") % 'bug-lilypond@gnu.org'),
		('\n')]
	map (sys.stdout.write, ls)
	
def setup_temp ():
	"""
	Create a temporary directory, and return its name. 
	"""
	global temp_dir
	if not keep_temp_dir_p:
		temp_dir = tempfile.mktemp (program_name)
	try:
		os.mkdir (temp_dir, 0777)
	except OSError:
		pass

	return temp_dir


def system (cmd, ignore_error = 0):
	"""Run CMD. If IGNORE_ERROR is set, don't complain when CMD returns non zero.

	RETURN VALUE

	Exit status of CMD
	"""
	
	if verbose_p:
		progress (_ ("Invoking `%s\'") % cmd)
	st = os.system (cmd)
	if st:
		name = re.match ('[ \t]*([^ \t]*)', cmd).group (1)
		msg = name + ': ' + _ ("command exited with value %d") % st
		if ignore_error:
			warning (msg + ' ' + _ ("(ignored)") + ' ')
		else:
			error (msg)

	return st


def cleanup_temp ():
	if not keep_temp_dir_p:
		if verbose_p:
			progress (_ ("Cleaning %s...") % temp_dir)
		shutil.rmtree (temp_dir)


def strip_extension (f, ext):
	(p, e) = os.path.splitext (f)
	if e == ext:
		e = ''
	return p + e

################################################################
# END Library
################################################################




class Duration:
	allowed_durs = (1, 2, 4, 8, 16, 32, 64, 128)
	def __init__ (self, clocks):
		self.clocks = clocks
		if clocks <= 0:
			self.clocks = duration_quant_clocks
		(self.dur, self.num, self.den) = self.dur_num_den (clocks)
		
	def dur_num_den (self, clocks):
		for i in range (len (allowed_tuplet_clocks)):
			if clocks == allowed_tuplet_clocks[i]:
				return allowed_tuplets[i]

		dur = 0; num = 1; den = 1;
		g = gcd (clocks, clocks_per_1)
		if g:
			(dur, num) = (clocks_per_1 / g, clocks / g)
		if not dur in self.allowed_durs:
			dur = 4; num = clocks; den = clocks_per_4
		return (dur, num, den)

	def dump (self):
		if self.den == 1:
			if self.num == 1:
				s = '%d' % self.dur
			elif self.num == 3 and self.dur != 1:
				s = '%d.' % (self.dur / 2)
			else:
				s = '%d*%d' % (self.dur, self.num)
		else:
			s = '%d*%d/%d' % (self.dur, self.num, self.den)
			
		global reference_note
		reference_note.duration = self

		return s

	def compare (self, other):
		return self.clocks - other.clocks

def sign (x):
	if x >= 0:
		return 1
	else:
		return -1

class Note:
	names = (0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6)
	alterations = (0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0)
	alteration_names = ('eses', 'es', '', 'is' , 'isis')
	
	def __init__ (self, clocks, pitch, velocity):
		self.pitch = pitch
		self.velocity = velocity
		# hmm
		self.clocks = clocks
		self.duration = Duration (clocks)
		(self.octave, self.notename, self.alteration) = self.o_n_a ()

	def o_n_a (self):
		# major scale: do-do
		# minor scale: la-la  (= + 5) '''

		n = self.names[(self.pitch) % 12]
 		a = self.alterations[(self.pitch) % 12]

		if a and key.flats:
			a = - self.alterations[(self.pitch) % 12]
			n = (n - a) % 7

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
			# as -> gis
			if key.sharps == 0 and key.flats == 0 \
			   and n == 5 and a == -1:
				n = 4; a = 1
			# des -> cis
       			elif key.flats == 1 and n == 1 and a == -1:
				n = 0; a = 1
			# ges -> fis
			elif key.flats == 2 and n == 4 and a == -1:
				n = 3; a = 1
			# g -> fisis
			elif key.sharps == 5 and n == 4 and a == 0:
				n = 3; a = 2
			# d -> cisis
			elif key.sharps == 6 and n == 1 and a == 0:
				n = 0; a = 2
			# a -> gisis
			elif key.sharps == 7 and n == 5 and a == 0:
				n = 4; a = 2

		# b -> ces
		if key.flats >= 6 and n == 6 and a == 0:
 			n = 0; a = -1; o = o + 1
		# e -> fes
		if key.flats >= 7 and n == 2 and a == 0:
 			n = 3; a = -1

		# f -> eis
		if key.sharps >= 3 and n == 3 and a == 0:
 			n = 2; a = 1
		# c -> bis
		if key.sharps >= 4 and n == 0 and a == 0:
 			n = 6; a = 1; o = o - 1

		return (o, n, a)
		
	def dump (self):
		s = chr ((self.notename + 2)  % 7 + ord ('a'))
		s = s + self.alteration_names[self.alteration + 2]
		if absolute_p:
			commas = self.octave
		else:
			delta = self.pitch - reference_note.pitch
			commas = sign (delta) * (abs (delta) / 12)
			if ((sign (delta) \
			     * (self.notename - reference_note.notename) + 7) \
			    % 7 >= 4) \
			    or ((self.notename == reference_note.notename) \
				and (abs (delta) > 4) and (abs (delta) < 12)):
				commas = commas + sign (delta)
			
		if commas > 0:
			s = s + "'" * commas
		elif commas < 0:
			s = s + "," * -commas

		if explicit_durations_p \
		   or Duration.compare (self.duration, reference_note.duration):
			s = s + self.duration.dump ()

		global reference_note
		reference_note = self
		
		# TODO: move space
		return s + ' '


class Time:
	def __init__ (self, num, den):
		self.clocks = 0
		self.num = num
		self.den = den

	def bar_clocks (self):
		return clocks_per_1 * self.num / self.den
	
	def dump (self):
		global time
		time = self
		return '\n  ' + '\\time %d/%d ' % (self.num, self.den) + '\n  '

class Tempo:
	def __init__ (self, seconds_per_1):
		self.clocks = 0
		self.seconds_per_1 = seconds_per_1

	def dump (self):
		return '\n  ' + '\\tempo 4 = %d ' % (4 * 60 / self.seconds_per_1) + '\n  '

class Clef:
	clefs = ('"bass_8"', 'bass', 'violin', '"violin^8"')
	def __init__ (self, type):
		self.type = type
		
	def dump (self):
		return '\n  \\clef %s\n  ' % self.clefs[self.type]

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
				name = name + Note.alteration_names[a + 2]

			s = '\\key ' + name
			if self.minor:
				s = s + ' \\minor'
			else:
				s = s + ' \\major'

		return '\n\n  ' + s + '\n  '


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
		# urg, we should be sure that we're in a lyrics staff
		if self.type == midi.LYRIC:
			s = '"%s"' % self.text
			d = Duration (self.clocks)
			if explicit_durations_p \
			   or Duration.compare (d,
						reference_note.duration):
				s = s + Duration (self.clocks).dump ()
			s = s + ' '
		else:
			s = '\n  % [' + self.text_types[self.type] + '] ' + self.text + '\n  '
		return s


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

def end_note (pitches, notes, t, e):
	try:
		(lt, vel) = pitches[e]
		del pitches[e]

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
			    (lt, Note (d, e, vel)))

	except KeyError:
		pass

def events_on_channel (channel):
	pitches = {}

	notes = []
	events = []
	last_lyric = 0
	last_time = 0
	for e in channel:
		t = e[0]

		if start_quant_clocks:
			t = quantise_clocks (t, start_quant_clocks)


		if e[1][0] == midi.NOTE_OFF \
		   or (e[1][0] == midi.NOTE_ON and e[1][2] == 0):
			end_note (pitches, notes, t, e[1][1])
			
		elif e[1][0] == midi.NOTE_ON:
			if not pitches.has_key (e[1][1]):
				pitches[e[1][1]] = (t, e[1][2])
				
		# all include ALL_NOTES_OFF
		elif e[1][0] >= midi.ALL_SOUND_OFF \
		     and e[1][0] <= midi.POLY_MODE_ON:
			for i in pitches.keys ():
				end_note (pitches, notes, t, i)
				
		elif e[1][0] == midi.META_EVENT:
			if e[1][1] == midi.END_OF_TRACK:
				for i in pitches.keys ():
					end_note (pitches, notes, t, i)
				break

			elif e[1][1] == midi.SET_TEMPO:
				(u0, u1, u2) = map (ord, e[1][2])
				us_per_4 = u2 + 256 * (u1 + 256 * u0)
				seconds_per_1 = us_per_4 * 4 / 1e6
				events.append ((t, Tempo (seconds_per_1)))
			elif e[1][1] == midi.TIME_SIGNATURE:
				(num, dur, clocks4, count32) = map (ord, e[1][2])
				den = 2 ** dur 
				events.append ((t, Time (num, den)))
			elif e[1][1] == midi.KEY_SIGNATURE:
 				(alterations, minor) = map (ord, e[1][2])
				sharps = 0
				flats = 0
				if alterations < 127:
					sharps = alterations
				else:
					flats = 256 - alterations

				k = Key (sharps, flats, minor)
				events.append ((t, k))

				# ugh, must set key while parsing
				# because Note init uses key
				# Better do Note.calc () at dump time?
				global key
				key = k

			elif e[1][1] == midi.LYRIC \
			     or (text_lyrics_p and e[1][1] == midi.TEXT_EVENT):
				if last_lyric:
					last_lyric.clocks = t - last_time
					events.append ((last_time, last_lyric))
				last_time = t
				last_lyric = Text (midi.LYRIC, e[1][2])

			elif e[1][1] >= midi.SEQUENCE_NUMBER \
			     and e[1][1] <= midi.CUE_POINT:
				events.append ((t, Text (e[1][1], e[1][2])))
			else:
				if verbose_p:
					sys.stderr.write ("SKIP: %s\n" % `e`)
				pass
		else:
			if verbose_p:
				sys.stderr.write ("SKIP: %s\n" % `e`)
			pass

	if last_lyric:
		# last_lyric.clocks = t - last_time
		# hmm
		last_lyric.clocks = clocks_per_4
		events.append ((last_time, last_lyric))
		last_lyric = 0
		
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
			if e[1].__class__ == Note \
			   and ((t == start_busy_t \
				 and e[1].clocks + t == end_busy_t) \
			    or t >= end_busy_t):
				thread.append (e)
				start_busy_t = t
				end_busy_t = t + e[1].clocks
			elif e[1].__class__ == Time \
			     or e[1].__class__ == Key \
			     or e[1].__class__ == Text \
			     or e[1].__class__ == Tempo:
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
	
def dump_skip (skip, clocks):
	return skip + Duration (clocks).dump () + ' '

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
		global reference_note
		s = s + '<'
		s = s + notes[0].dump ()
		r = reference_note
		for i in notes[1:]:
			s = s + i.dump ()
		s = s + '>'
		reference_note = r
	return s

def dump_bar_line (last_bar_t, t, bar_count):
	s = ''
	bar_t = time.bar_clocks ()
	if t - last_bar_t >= bar_t:
		bar_count = bar_count + (t - last_bar_t) / bar_t
		
		if t - last_bar_t == bar_t:
			s = '|\n  %% %d\n  ' % bar_count
			last_bar_t = t
		else:
			# urg, this will barf at meter changes
			last_bar_t = last_bar_t + (t - last_bar_t) / bar_t * bar_t
			
	return (s, last_bar_t, bar_count)

			
def dump_channel (thread, skip):
	global key, reference_note, time

	key = Key (0, 0, 0)
	time = Time (4, 4)
	# urg LilyPond doesn't start at c4, but
	# remembers from previous tracks!
	# reference_note = Note (clocks_per_4, 4*12, 0)
	reference_note = Note (0, 4*12, 0)
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
	last_bar_t = 0
	bar_count = 1
	
	lines = ['']
	for ch in chs: 
		t = ch[0]

		i = string.rfind (lines[-1], '\n') + 1
		if len (lines[-1][i:]) > LINE_BELL:
			lines.append ('')
			
		if t - last_t > 0:
			lines[-1] = lines[-1] + dump_skip (skip, t-last_t)
		elif t - last_t < 0:
			errorport.write ('BUG: time skew')

		(s, last_bar_t, bar_count) = dump_bar_line (last_bar_t,
							    t, bar_count)
		lines[-1] = lines[-1] + s
		
		lines[-1] = lines[-1] + dump_chord (ch[1])

		clocks = 0
		for i in ch[1]:
			if i.clocks > clocks:
				clocks = i.clocks
				
		last_t = t + clocks
		
		(s, last_bar_t, bar_count) = dump_bar_line (last_bar_t,
							    last_t, bar_count)
		lines[-1] = lines[-1] + s

	return string.join (lines, '\n  ') + '\n'

def track_name (i):
	return 'track%c' % (i + ord ('A'))

def channel_name (i):
	return 'channel%c' % (i + ord ('A'))

def dump_track (channels, n):
	s = '\n'
	track = track_name (n)
	clef = guess_clef (channels)

	for i in range (len (channels)):
		channel = channel_name (i)
		item = thread_first_item (channels[i])

		if item and item.__class__ == Note:
			skip = 's'
			s = s + '%s = \\notes' % (track + channel)
			if not absolute_p:
				s = s + '\\relative c '
		elif item and item.__class__ == Text:
			skip = '" "'
			s = s + '%s = \\lyrics ' % (track + channel)
		else:
			skip = '\\skip '
			# must be in \notes mode for parsing \skip
			s = s + '%s = \\notes ' % (track + channel)
		s = s + '{\n'
		s = s + '  ' + dump_channel (channels[i][0], skip)
		s = s + '}\n\n'

	s = s + '%s = <\n' % track

	if clef.type != 2:
		s = s + clef.dump () + '\n'

	for i in range (len (channels)):
		channel = channel_name (i)
		item = thread_first_item (channels[i])
		if item and item.__class__ == Text:
			s = s + '  \\context Lyrics = %s \\%s\n' % (channel,
								    track + channel)
		else:
			s = s + '  \\context Voice = %s \\%s\n' % (channel,
								   track + channel)
	s = s + '>\n\n'
	return s

def thread_first_item (thread):
	for chord in thread:
		for event in chord:
			if event[1].__class__ == Note \
			   or (event[1].__class__ == Text \
			       and event[1].type == midi.LYRIC):
				return event[1]
	return 0

def track_first_item (track):
	for thread in track:
		return thread_first_item (thread)

def guess_clef (track):
	i = 0
	p = 0
	for thread in track:
		for chord in thread:
			for event in chord:
				if event[1].__class__ == Note:
					i = i + 1
					p = p + event[1].pitch
	if i and p / i <= 3*12:
		return Clef (0)
	elif i and p / i <= 5*12:
		return Clef (1)
	elif i and p / i >= 7*12:
		return Clef (3)
	else:
		return Clef (2)
	

def convert_midi (f, o):
	global clocks_per_1, clocks_per_4, key

	str = open (f).read ()
	midi_dump = midi.parse (str)

	clocks_per_1 = midi_dump[0][1]
	clocks_per_4 = clocks_per_1 / 4
	
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
		key = Key (0, 0, 0)
		tracks.append (split_track (t))

	tag = '%% Lily was here -- automatically converted by %s from %s' % ( program_name, f)

	s = ''
	s = tag + '\n\n'
	for i in range (len (tracks)):
		s = s + dump_track (tracks[i], i)

	s = s + '\n\\score {\n  <\n'
	for i in range (len (tracks)):
		track = track_name (i)
		item = track_first_item (tracks[i])
		if item and item.__class__ == Note:
			s = s + '    \\context Staff=%s \\%s\n' % (track, track)
		elif item and item.__class__ == Text:
			s = s + '    \\context Lyrics=%s \\%s\n' % (track, track)
	s = s + '  >\n}\n'

 	progress (_ ("%s output to `%s'...") % ('LY', o))

	if o == '-':
		h = sys.stdout
	else:
		h = open (o, 'w')

	h.write (s)
	h.close ()


(sh, long) = getopt_args (option_definitions)
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


	elif o == '--absolute-pitches' or o == '-a':
		global absolute_p
		absolute_p = 1
	elif o == '--duration-quant' or o == '-d':
		global duration_quant
		duration_quant = string.atoi (a)
	elif o == '--explicit-durations' or o == '-e':
		global explicit_durations_p
		explicit_durations_p = 1
	elif o == '--key' or o == '-k':
		(alterations, minor) = map (string.atoi, string.split (a + ':0', ':'))[0:2]
 		sharps = 0
 		flats = 0
 		if alterations >= 0:
 			sharps = alterations
 		else:
 			flats = - alterations
		global key
		key = Key (sharps, flats, minor)
	elif o == '--start-quant' or o == '-s':
		start_quant = string.atoi (a)
	elif o == '--allow-tuplet' or o == '-t':
		a = string.replace (a, '/', '*')
		tuplet = map (string.atoi, string.split (a, '*'))
		allowed_tuplets.append (tuplet)
	# lots of midi files use plain text for lyric events
	elif o == '--text-lyrics' or o == '-x':
		text_lyrics_p = 1


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

	if outdir != '.' and outdir != '':
		try:
			os.mkdir (outdir, 0777)
		except OSError:
			pass

	convert_midi (f, o)

