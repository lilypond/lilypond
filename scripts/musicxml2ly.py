import sys
import re
import os

datadir = '@local_lilypond_datadir@'
if not os.path.isdir (datadir):
	datadir = '@lilypond_datadir@'
if os.environ.has_key ('LILYPONDPREFIX'):
	datadir = os.environ['LILYPONDPREFIX']
	while datadir[-1] == os.sep:
		datadir = datadir[:-1]

if os.path.exists (os.path.join (datadir, 'share/lilypond/@TOPLEVEL_VERSION@/')):
	datadir = os.path.join (datadir, 'share/lilypond/@TOPLEVEL_VERSION@/')

sys.path.insert (0, os.path.join (datadir, 'python'))

import musicxml
import musicexp
from rational import Rational

def musicxml_duration_to_lily (mxl_note):
	d = musicexp.Duration ()
	if mxl_note.get_maybe_exist_typed_child (musicxml.Type):
		d.duration_log = mxl_note.get_duration_log ()
	else:
		d.factor = mxl_note._duration
		d.duration_log = 0

	d.dots = len (mxl_note.get_typed_children (musicxml.Dot))
	d.factor = mxl_note._duration / d.get_length ()

	return d 	
	
def musicxml_voice_to_lily_voice (voice):
	
	ly_voice = []
	ly_now = Rational (0)
	for n in voice:
		if not n.__class__.__name__ == 'Note':
			print 'not a note?'
			continue

		pitch  = None
		duration = None
		
		mxl_pitch = n.get_maybe_exist_typed_child (musicxml.Pitch)
		event = None
		
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
			#print 'append'
		ev_chord = ly_voice[-1]
		ev_chord.elements.append (event)


	seq_music = musicexp.SequentialMusic()

	
	seq_musicexp.elements = ly_voice
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
	p.octave = mxl_pitch.get_octave () -4
	return p

def get_all_voices (parts):
	all_voices = {} 
	for p in parts:
		p.interpret ()
		p.extract_voices ()		
		voice_dict = p.get_voices ()
		
		for (id, voice) in voice_dict.items ():
			m = musicxml_voice_to_lily_voice (voice)
			m_name = 'Part' + p.name + 'Voice' + id
			m_name = musicxml_id_to_lily (m_name)
			all_voices[m_name] = m

	return all_voices

printer = musicexp.Output_printer()


tree = musicxml.read_musicxml (sys.argv[1])
parts = tree.get_typed_children (musicxml.Part)

voices = get_all_voices (parts)
for  (k,v) in voices.items():
	print '%s = \n' % k
	v.print_ly (printer.dump)
	printer.newline()
	

