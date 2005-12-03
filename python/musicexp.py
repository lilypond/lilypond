import inspect
import sys
import string
from rational import Rational

def flatten_list (fl):
	if type(fl) == type((1,)):
		return 
	
	flattened = []
	for f in fl:
		flattened += flatten_list (fl)
	
def is_derived (deriv_class, maybe_base):
	if deriv_class == maybe_base:
		return True

	for c in deriv_class.__bases__:
		if is_derived (c, maybe_base):
			return True

	return False

class Output_printer:
	def __init__ (self):
		self.line = ''
		self.indent = 0
		self.file = sys.stdout
		self.line_len = 72
		
	def add_word (self, str):
		if (len (str) + 1 + len (self.line) > self.line_len):
			self.newline()

		self.indent += str.count ('<') + str.count ('{')
		self.indent -= str.count ('>') + str.count ('}')
		self.line += ' ' + str
		
	def newline (self):
		self.file.write (self.line + '\n')
		self.line = ' ' * self.indent
		
	def dump (self, str):
		words = string.split (str)
		for w in words:
			self.add_word (w)
		
class Duration:
	def __init__ (self):
		self.duration_log = 2
		self.dots = 0
		self.factor = Rational (1)

	def lisp_expression (self):
		return '(ly:make-duration %d %d %d %d)' % (self.duration_log,
							   self.dots,
							   self.factor.numerator (),
							   self.factor.denominator ())

	def ly_expression (self):
		str = '%d%s' % (1 << self.duration_log, '.'*self.dots)

		if self.factor <> Rational (1,1):
			str += '*%d/%d' % (self.factor.numerator (),self.factor.denominator ())

		return str

	def copy (self):
		d = Duration ()
		d.dots = self.dots
		d.duration_log = self.duration_log
		d.factor = self.factor
		return d

	def get_length (self):
		dot_fact = Rational( (1 << (1 + self.dots))-1,
				     1 << self.dots)

		log = abs (self.duration_log)
		dur = 1 << log
		if self.duration_log < 0:
			base = Rational (dur)
		else:
			base = Rational (1, dur)

		return base * dot_fact * self.factor
	
class Pitch:
	def __init__ (self):
		self.alteration = 0
		self.step = 0
		self.octave = 0

	def lisp_expression (self):
		return '(ly:make-pitch %d %d %d)' % (self.octave,
						     self.step,
						     self.alteration)

	def copy (self):
		p = Pitch ()
		p.alteration = self.alteration
		p.step = self.step
		p.octave = self.octave 
		return p

	def steps (self):
		return self.step + self.octave * 7
	
	def ly_step_expression (self): 
		str = 'cdefgab'[self.step]
		if self.alteration > 0:
			str += 'is'* (self.alteration)
		elif self.alteration < 0:
			str += 'es'* (-self.alteration)

		return str.replace ('aes', 'as').replace ('ees', 'es')
	
	def ly_expression (self):
		str = self.ly_step_expression ()
		if self.octave >= 0:
			str += "'" * (self.octave + 1) 
		elif self.octave < -1:
			str += "," * (-self.octave - 1) 
			
		return str

class Music:
	def __init__ (self):
		self.tag = None
		self.parent = None
		self.start = Rational (0)
		pass

	def get_length(self):
		return Rational (0)
	
	def set_tag (self, counter, tag_dict):
		self.tag = counter
		tag_dict [counter] = self
		return counter + 1
	
	def get_properties (self):
		return ''
	
	def has_children (self):
		return False
	
	def get_index (self):
		if self.parent:
			return self.parent.elements.index (self)
		else:
			return None
		
	def lisp_expression (self):
		name = self.name()
		tag = ''
		if self.tag:
			tag = "'input-tag %d" % self.tag

		props = self.get_properties ()
#		props += 'start %f ' % self.start
		
		return "(make-music '%s %s %s)" % (name, tag,  props)

	def set_start (self, start):
		self.start = start

	def find_first (self, predicate):
		if predicate (self):
			return self
		return None

	def print_ly (self, printer):
		printer (self.ly_expression ())
		
class Music_document:
	def __init__ (self):
		self.music = test_expr ()
		self.tag_dict = {}
		self.touched = True
		
	def recompute (self):
		self.tag_dict = {}
		self.music.set_tag (0, self.tag_dict)
		self.music.set_start (Rational (0))
		
class NestedMusic(Music):
	def __init__ (self):
		Music.__init__ (self)
		self.elements = [] 
	def has_children (self):
		return self.elements
	def set_tag (self, counter, dict):
		counter = Music.set_tag (self, counter, dict)
		for e in self.elements :
			counter = e.set_tag (counter, dict)
		return counter

	def insert_around (self, succ, elt, dir):
		assert elt.parent == None
		assert succ == None or succ in self.elements

		
		idx = 0
		if succ:
			idx = self.elements.index (succ)
			if dir > 0:
				idx += 1
		else:
			if dir < 0:
				idx = 0
			elif dir > 0:
				idx = len (self.elements)

		self.elements.insert (idx, elt)
		elt.parent = self
		
	def get_properties (self):
		return ("'elements (list %s)"
			% string.join (map (lambda x: x.lisp_expression(),
					    self.elements)))

	def get_subset_properties (self, predicate):
		return ("'elements (list %s)"
			% string.join (map (lambda x: x.lisp_expression(),
					    filter ( predicate,  self.elements))))
	def get_neighbor (self, music, dir):
		assert music.parent == self
		idx = self.elements.index (music)
		idx += dir
		idx = min (idx, len (self.elements) -1)
		idx = max (idx, 0)

		return self.elements[idx]

	def delete_element (self, element):
		assert element in self.elements
		
		self.elements.remove (element)
		element.parent = None
		
	def set_start (self, start):
		self.start = start
		for e in self.elements:
			e.set_start (start)

	def find_first (self, predicate):
		r = Music.find_first (self, predicate)
		if r:
			return r
		
		for e in self.elements:
			r = e.find_first (predicate)
			if r:
				return r
		return None
		
class SequentialMusic (NestedMusic):
	def name(self):
		return 'SequentialMusic'
	
	def print_ly (self, printer):
		printer ('{')
		for e in self.elements:
			e.print_ly (printer)
		printer ('}')

	def lisp_sub_expression (self, pred):
		name = self.name()
		tag = ''
		if self.tag:
			tag = "'input-tag %d" % self.tag


		props = self.get_subset_properties (pred)
		
		return "(make-music '%s %s %s)" % (name, tag,  props)
	
	def set_start (self, start):
		for e in self.elements:
			e.set_start (start)
			start += e.get_length()
			
class EventChord(NestedMusic):
	def name(self):
		return "EventChord"

	def get_length (self):
		l = Rational (0)
		for e in self.elements:
			l = max(l, e.get_length())
		return l
	
	def print_ly (self, printer):
		note_events = [e for e in self.elements if
			       is_derived (e.__class__, NoteEvent)]
		rest_events = [e for e in self.elements if
			       is_derived (e.__class__, RhythmicEvent)
			       and not is_derived (e.__class__, NoteEvent)]
		
		other_events = [e for e in self.elements if
				not is_derived (e.__class__, RhythmicEvent)]

		if rest_events:
			printer (rest_events[0].ly_expression ())
		elif len (note_events) == 1:
			printer (note_events[0].ly_expression ())
		elif note_events:
			pitches = [x.pitch.ly_expression () for x in note_events]
			printer ('<%s>' % string.join (pitches)
				 + note_events[0].duration.ly_expression ())
		else:
			pass
		#	print  'huh', rest_events, note_events, other_events
			
		for e in other_events:
			e.print_ly (printer)
		
			
class Event(Music):
	def __init__ (self):
		Music.__init__ (self)

	def name (self):
		return "Event"

class ArpeggioEvent(Music):
	def name (self):
		return 'ArpeggioEvent'
	
	def ly_expression (self):
		return ('\\arpeggio')
	
class RhythmicEvent(Event):
	def __init__ (self):
		Event.__init__ (self)
		self.duration = Duration()
		
	def get_length (self):
		return self.duration.get_length()
		
	def get_properties (self):
		return ("'duration %s"
			% self.duration.lisp_expression ())
	
	def name (self):
		return 'RhythmicEvent'

class RestEvent (RhythmicEvent):
	def name (self):
		return 'RestEvent'
	def ly_expression (self):
		return 'r%s' % self.duration.ly_expression ()

class SkipEvent (RhythmicEvent):
	def name (self):
		return 'SkipEvent'
	def ly_expression (self):
		return 's%s' % self.duration.ly_expression () 

class NoteEvent(RhythmicEvent):
	def  __init__ (self):
		RhythmicEvent.__init__ (self)
		self.pitch = Pitch()

	def name (self):
		return 'NoteEvent'
	
	def get_properties (self):
		return ("'pitch %s\n 'duration %s"
			% (self.pitch.lisp_expression (),
			   self.duration.lisp_expression ()))

	def ly_expression (self):
		return '%s%s' % (self.pitch.ly_expression (),
				 self.duration.ly_expression ())



class KeySignatureEvent (Event):
	def __init__ (self, tonic, scale):
		Event.__init__ (self)
		self.scale = scale
		self.tonic = tonic
	def name (self):
		return 'KeySignatureEvent'
	def ly_expression (self):
		return '\\key %s \\major' % self.tonic.ly_step_expression ()
	
	def lisp_expression (self):
		pairs = ['(%d . %d)' % (i , self.scale[i]) for i in range (0,7)]
		scale_str = ("'(%s)" % string.join (pairs))

		return """ (make-music 'KeyChangeEvent
          'pitch-alist %s) """ % scale_str

class ClefEvent (Event):
	def __init__ (self, t):
		Event.__init__ (self)
		self.type = t
		
	def name (self):
		return 'ClefEvent'
	def ly_expression (self):
		return '\\clef "%s"' % self.type
	clef_dict = {
		"G": ("clefs.G", -2, -6),
		"C": ("clefs.C", 0, 0),
		"F": ("clefs.F", 2, 6),
		}
	
	def lisp_expression (self):
		(glyph, pos, c0) = self.clef_dict [self.type]
		clefsetting = """
		(make-music 'SequentialMusic
		'elements (list
      (context-spec-music
       (make-property-set 'clefGlyph "%s") 'Staff)
      (context-spec-music
       (make-property-set 'clefPosition %d) 'Staff)
      (context-spec-music
       (make-property-set 'middleCPosition %d) 'Staff)))
""" % (glyph, pos, c0)
		return clefsetting

def test_expr ():
	m = SequentialMusic()
	l = 2  
	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 1
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 3
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

 	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 2 
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

 	evc = ClefEvent("G")
	m.insert_around (None, evc, 0)

 	evc = EventChord()
	tonic = Pitch ()
	tonic.step = 2
	tonic.alteration = -2
	n = KeySignatureEvent(tonic, [0, 0, -2, 0, 0,-2,-2]  )
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

	return m


if __name__ == '__main__':
	expr = test_expr()
	expr.set_start (Rational (0))
	print expr.ly_expression()
	start = Rational (0,4)
	stop = Rational (4,2)
	def sub(x, start=start, stop=stop):
		ok = x.start >= start and x.start +x.get_length() <= stop
		return ok
	
	print expr.lisp_sub_expression(sub)

