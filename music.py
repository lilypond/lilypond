import string

		
class Duration:
	def __init__ (self):
		self.duration_log = 2
		self.dots = 0
		self.factor = (1,1)

	def lisp_expression (self):
		return '(ly:make-duration %d %d %d %d)' % (self.duration_log,
							   self.dots,
							   self.factor[0],
							   self.factor[1])

	def ly_expression (self):
		str = '%d%s' % (1 << self.duration_log, '.'*self.dots)

		if self.factor <> (1,1):
			str += '*%d/%d' % self.factor
		return str

	def copy (self):
		d = Duration ()
		d.dots = self.dots
		d.duration_log = self.duration_log
		d.factor = self.factor
		return d

	def length (self):
		dot_fact = ((1 << (1 + self.dots))-1.0)/ (1 << self.dots)

		log = abs (self.duration_log)
		dur = 1 << log
		if self.duration_log < 0:
			base = 1.0 * dur
		else:
			base = 1.0 / dur

		return base * dot_fact * self.factor[0]/self.factor[1]
	
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
	
	def ly_expression (self):

		str = 'cdefgab'[self.step]
		if self.alteration > 0:
			str += 'is'* (self.alteration/2)
		elif self.alteration < 0:
			str += 'es'* (-self.alteration/2)

		if self.octave >= 0:
			str += "'" * (self.octave + 1) 
		elif self.octave < -1:
			str += "," * (-self.octave - 1) 
			
		return str

class Music:
	def __init__ (self):
		self.tag = None
		self.parent = None
		self.start = 0.0
		pass

	def length(self):
		return 0.0
	
	def set_tag (self, counter, tag_dict):
		self.tag = counter
		tag_dict [counter] = self
		return counter + 1
	
	def get_properties (self):
		return ''
	
	def lisp_expression (self):
		name = self.name()
		tag = ''
		if self.tag:
			tag = "'input-tag %d" % self.tag

		props = self.get_properties ()
#		props += 'start %f ' % self.start
		
		return "(make-music '%s %s %s)" % (name, tag,  props)

	def set_start (self, start):
		start = round (start * 384) / 384.0
		self.start = start

	def find_first (self, predicate):
		if predicate (self):
			return self
		return None
	 
class Music_document:
	def __init__ (self):
		self.music = test_expr ()
		self.tag_dict = {}
		self.touched = True
		
	def recompute (self):
		self.tag_dict = {}
		self.music.set_tag (0, self.tag_dict)
		self.music.set_start (0.0)
		
class NestedMusic(Music):
	def __init__ (self):
		Music.__init__ (self)
		self.elements = [] 

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
	
	def ly_expression (self):
		return '{ %s }' % string.join (map (lambda x:x.ly_expression(),
						    self.elements)) 
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
			start += e.length()
			
class EventChord(NestedMusic):
	def name(self):
		return "EventChord"

	def length (self):
		l = 0.0
		for e in self.elements:
			l = max(l, e.length())
		return l
	
	def ly_expression (self):
		str = string.join (map (lambda x: x.ly_expression(),
					self.elements))
		if len (self.elements) > 1:
			str = '<<%s>>' % str
		
		return str
			
class Event(Music):
	def __init__ (self):
		Music.__init__ (self)

	def name (self):
		return "Event"

class RhythmicEvent(Event):
	def __init__ (self):
		Event.__init__ (self)
		self.duration = Duration()
		
	def length (self):
		return self.duration.length()
		
	def get_properties (self):
		return ("'duration %s"
			% self.duration.lisp_expression ())
	
	def name (self):
		return 'RhythmicEvent'

class RestEvent (RhythmicEvent):
	def name (self):
		return 'RestEvent'
	def ly_expression (self):
		return '%s' % self.duration.ly_expression ()

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

def test_expr ():
	m = SequentialMusic()
	l =2 
	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 1
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

 	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 2 
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

	evc = EventChord()
	n = NoteEvent()
	n.duration.duration_log = l
	n.pitch.step = 3
	evc.insert_around (None, n, 0)
	m.insert_around (None, evc, 0)

	return m


if __name__ == '__main__':
	expr = test_expr()
	expr.set_start (0.0)

	start = 0.25
	stop = 0.5
	def sub(x, start=start, stop=stop):
		ok = x.start >= start and x.start +x.length() <= stop
		return ok
	
	print expr.lisp_sub_expression(sub)

