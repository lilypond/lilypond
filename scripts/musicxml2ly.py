#!@PYTHON@

import optparse
import sys
import re
import os
import string
from gettext import gettext as _

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX'):
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir = datadir[:-1]

if os.path.exists (os.path.join (datadir, 'share/lilypond/@TOPLEVEL_VERSION@/')):
	datadir = os.path.join (datadir, 'share/lilypond/@TOPLEVEL_VERSION@/')
elif os.path.exists (os.path.join (datadir, 'share/lilypond/current/')):
	datadir = os.path.join (datadir, 'share/lilypond/current/')

sys.path.insert (0, os.path.join (datadir, 'python'))


import musicxml
import musicexp
from rational import Rational

def musicxml_duration_to_lily (mxl_note):
	d = musicexp.Duration ()
	if mxl_note.get_maybe_exist_typed_child (musicxml.Type):
		d.duration_log = mxl_note.get_duration_log ()
	else:
		d.duration_log = 0

	d.dots = len (mxl_note.get_typed_children (musicxml.Dot))
	d.factor = mxl_note._duration / d.get_length ()

	return d 	

span_event_dict = {
	'start': -1,
	'stop': 1
}

def group_tuplets (music_list, events):
	indices = []

	j = 0
	for (ev_chord, tuplet_elt, fraction) in events:
		while (j < len (music_list)):
			if music_list[j]== ev_chord:
				break
			j += 1
		if tuplet_elt.type == 'start':
			indices.append ((j, None, fraction))
		elif tuplet_elt.type == 'stop':
			indices[-1] = (indices[-1][0], j, indices[-1][2])

	new_list = []
	last = 0
	for (i1, i2, frac) in indices:
		if i1 >= i2:
			continue

		new_list.extend (music_list[last:i1])
		seq = musicexp.SequentialMusic ()
		last = i2 + 1
		seq.elements = music_list[i1:last]

		tsm = musicexp.TimeScaledMusic ()
		tsm.element = seq

		tsm.numerator = frac[0]
		tsm.denominator  = frac[1]

		new_list.append (tsm)

	new_list.extend (music_list[last:])
	return new_list

def musicxml_clef_to_lily (mxl):
	sign = mxl.get_maybe_exist_named_child ('sign')
	change = musicexp.ClefChange ()
	if sign:
		change.type = sign.get_text ()
	return change

	
def musicxml_time_to_lily (mxl):
	beats = mxl.get_maybe_exist_named_child ('beats')
	type = mxl.get_maybe_exist_named_child ('beat-type')
	change = musicexp.TimeSignatureChange()
	change.fraction = (string.atoi(beats.get_text ()),
			   string.atoi(type.get_text ()))
	
	return change

def musicxml_key_to_lily (mxl):
	mode = mxl.get_maybe_exist_named_child ('mode').get_text ()
	fifths = string.atoi (mxl.get_maybe_exist_named_child ('fifths').get_text ())

	fifth = musicexp.Pitch()
	fifth.step = 4
	if fifths < 0:
		fifths *= -1
		fifth.step *= -1
		fifth.normalize ()
	
	c = musicexp.Pitch()
	for x in range (fifths):
		c = c.transposed (fifth)

	c.octave = 0

	change = musicexp.KeySignatureChange()
	change.mode = mode
	change.tonic = c
	return change
	
def musicxml_attributes_to_lily (attrs):
	elts = []
	attr_dispatch =  {
		'clef': musicxml_clef_to_lily,
		'time': musicxml_time_to_lily,
		'key': musicxml_key_to_lily
	}
	for (k, func) in attr_dispatch.items ():
		childs = attrs.get_named_children (k)

		## ugh: you get clefs spread over staves for piano
		if childs:
			elts.append (func (childs[0]))
	
	return elts

def insert_measure_start_comments (ly_voice, indices):
	idxs = indices[:]
	idxs.reverse ()
	for i in idxs:
		c = musicexp.Comment()
		c.text = '' 
		ly_voice.insert (i, c)

	return ly_voice
	
def musicxml_voice_to_lily_voice (voice):
	
	ly_voice = []
	ly_now = Rational (0)

	tuplet_events = []

	measure_start_indices = []
	for n in voice:
		if n.is_first ():
			measure_start_indices.append (len (ly_voice))
			
		if isinstance (n, musicxml.Attributes):
			ly_voice.extend (musicxml_attributes_to_lily (n))
			continue
		
		if not n.__class__.__name__ == 'Note':
			print 'not a Note or Attributes?'
			continue
		
		
		pitch  = None
		duration = None
		
		mxl_pitch = n.get_maybe_exist_typed_child (musicxml.Pitch)
		event = None

		notations = n.get_maybe_exist_typed_child (musicxml.Notations)
		tuplet_event = None
		slur_event = None
		if notations:
			tuplet_event = notations.get_tuplet ()
			slur_event = notations.get_slur ()
			
		if mxl_pitch:
			pitch = musicxml_pitch_to_lily (mxl_pitch)
			event = musicexp.NoteEvent()
			event.pitch = pitch
		elif n.get_maybe_exist_typed_child (musicxml.Rest):
			event = musicexp.RestEvent()

		event.duration = musicxml_duration_to_lily (n)
		ev_chord = None
		if None ==  n.get_maybe_exist_typed_child (musicxml.Chord):
			if ly_voice:
				ly_now += ly_voice[-1].get_length ()

			if ly_now <> n._when:
				diff = n._when - ly_now 
				if diff < Rational (0):
					print 'huh: negative skip', n._when, ly_now, n._duration
					diff = Rational (1,314159265)
				
				skip = musicexp.SkipEvent()
				skip.duration.duration_log = 0
				skip.duration.factor = diff

				evc = musicexp.EventChord ()
				evc.elements.append (skip)
				ly_voice.append (evc)
				ly_now = n._when
				
			ly_voice.append (musicexp.EventChord())
		else:
			pass

		ev_chord = ly_voice[-1]
		ev_chord.elements.append (event)

		if tuplet_event:
			mod = n.get_maybe_exist_typed_child (musicxml.Time_modification)
			frac = (1,1)
			if mod:
				frac = mod.get_fraction ()
				
			tuplet_events.append ((ev_chord, tuplet_event, frac))
			
		if slur_event:
			sp = musicexp.SlurEvent()
			try:
				sp.span_direction = span_event_dict[slur_event.type]
				ev_chord.elements.append (sp)
			except KeyError:
				pass

	ly_voice = insert_measure_start_comments (ly_voice, measure_start_indices)
	ly_voice = group_tuplets (ly_voice, tuplet_events)

	seq_music = musicexp.SequentialMusic()
	
	seq_music.elements = ly_voice
	return seq_music


def musicxml_id_to_lily (id):
	digits = ['one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight',
		  'nine', 'ten']
	
	for dig in digits:
		d = digits.index (dig) + 1
		dig = dig[0].upper() + dig[1:]
		id = re.sub ('%d' % d, dig, id)

	id = re.sub  ('[^a-zA-Z]', 'X', id)
	return id


def musicxml_pitch_to_lily (mxl_pitch):
	p = musicexp.Pitch()
	p.alteration = mxl_pitch.get_alteration ()
	p.step = (ord (mxl_pitch.get_step ()) - ord ('A') + 7 - 2) % 7
	p.octave = mxl_pitch.get_octave () - 4
	return p

def get_all_voices (parts):
	all_voices = {} 
	for p in parts:
		p.interpret ()
		p.extract_voices ()		
		voice_dict = p.get_voices ()
		
		for (id, voice) in voice_dict.items ():
			m = musicxml_voice_to_lily_voice (voice)
			m_name = 'Part' + p.id + 'Voice' + id
			m_name = musicxml_id_to_lily (m_name)
			all_voices[m_name] = m

	return all_voices

class NonDentedHeadingFormatter (optparse.IndentedHelpFormatter):
    def format_heading(self, heading):
	    if heading:
		    return heading[0].upper() + heading[1:] + ':\n'
	    return ''
    def format_option_strings(self, option):
	    sep = ' '
	    if option._short_opts and option._long_opts:
		    sep = ','

	    metavar = ''
	    if option.takes_value():
		    metavar = '=' + option.metavar or option.dest.upper()

	    return "%3s%s %s%s" % (" ".join (option._short_opts),
				   sep,
				   " ".join (option._long_opts),
				   metavar)

    def format_usage(self, usage):
        return _("Usage: %s\n") % usage
    
    def format_description(self, description):
	    return description
  

def option_parser ():
	p = optparse.OptionParser(usage='musicxml2ly FILE.xml',
				  version = """%prog (LilyPond) @TOPLEVEL_VERSION@

This program is free software.  It is covered by the GNU General Public
License and you are welcome to change it and/or distribute copies of it
under certain conditions.  Invoke as `lilypond --warranty' for more
information.

Copyright (c) 2005 by
  Han-Wen Nienhuys <hanwen@xs4all.nl> and
  Jan Nieuwenhuizen <janneke@gnu.org>
""",

				  description  =
				  """Convert MusicXML file to LilyPond input.
"""
				  )
	p.add_option ('-v', '--verbose',
		      action = "store_true",
		      dest = 'verbose',
		      help = 'be verbose')
	p.add_option ('-o', '--output',
		      metavar = 'FILE',
		      action = "store",
		      default = None,
		      type = 'string',
		      dest = 'output',
		      help = 'set output file')

	p.add_option_group  ('', description = '''Report bugs via http://post.gmane.org/post.php?group=gmane.comp.gnu.lilypond.bugs
''')
	p.formatter = NonDentedHeadingFormatter () 
	return p


def convert (filename, output_name):
	
	printer = musicexp.Output_printer()
	tree = musicxml.read_musicxml (filename)
	parts = tree.get_typed_children (musicxml.Part)

	voices = get_all_voices (parts)


	if output_name:
		printer.file = open (output_name,'w')
		
	for  (k,v) in voices.items():
		printer.dump ('%s = ' % k)
		v.print_ly (printer)
		printer.newline()

	return voices


opt_parser = option_parser()

(options, args) = opt_parser.parse_args ()
if options.version:
	opt_parser.print_version()
	sys.exit (0)
if not args:
	opt_parser.print_usage()
	sys.exit (2)

voices = convert (args[0], options.output)
