import sys
import new
import re
import string
from rational import Rational

from xml.dom import minidom, Node


class Xml_node:
	def __init__ (self):
		self._children = []
		self._data = None
		self._original = None
		self._name = 'xml_node'
		self._parent = None

	def is_first (self):
		return self._parent.get_typed_children (self.__class__)[0] == self

	def original (self):
		return self._original 
	def get_name (self):
		return self._name
	
	def get_text (self):
		if self._data:
			return self._data

		if not self._children:
			return ''
		
		return ''.join ([c.get_text () for c in self._children])

	def get_typed_children (self, klass):
		return [c for c in self._children if isinstance(c, klass)]
	
	def get_named_children (self, nm):
		return self.get_typed_children (class_dict[nm])

	def get_children (self, predicate):
		return [c for c in self._children if predicate(c)]

	def get_all_children (self):
		return self._children

	def get_maybe_exist_named_child (self, name):
		return self.get_maybe_exist_typed_child (class_dict[name])
	
	def get_maybe_exist_typed_child (self, klass):
		cn = self.get_typed_children (klass)
		if len (cn)==0:
			return None
		elif len (cn) == 1:
			return cn[0]
		else:
			raise "More than 1 child", klass
		
	def get_unique_typed_child (self, klass):
		cn = self.get_typed_children(klass)
		if len (cn) <> 1:
			print self.__dict__ 
			raise 'Child is not unique for', (klass, 'found', cn)

		return cn[0]
	
class Music_xml_node (Xml_node):
	def __init__ (self):
		Xml_node.__init__ (self)
		self.duration = Rational (0)
		self.start = Rational (0)
		

class Duration (Music_xml_node):
	def get_length (self):
		dur = string.atoi (self.get_text ()) * Rational (1,4)
		return dur
		
class Hash_comment (Music_xml_node):
	pass
		
class Pitch (Music_xml_node):
	def get_step (self):
		ch = self.get_unique_typed_child (class_dict[u'step'])
		step = ch.get_text ().strip ()
		return step
	def get_octave (self):
		ch = self.get_unique_typed_child (class_dict[u'octave'])

		step = ch.get_text ().strip ()
		return string.atoi (step)
	
	def get_alteration (self):
		ch = self.get_maybe_exist_typed_child (class_dict[u'alter'])
		alter = 0
		if ch:
			alter = string.atoi (ch.get_text ().strip ())
		return alter

class Measure_element (Music_xml_node):
	def get_voice_id (self):
		voice_id = self.get_maybe_exist_named_child ('voice')
		if voice_id:
			return voice_id.get_text ()
		else:
			return None
		
	def is_first (self):
		cn = self._parent.get_typed_children (self.__class__)
		cn = [c for c in cn if c.get_voice_id () == self.get_voice_id ()]
		return cn[0] == self
	
class Attributes (Measure_element):
	def __init__ (self):
		Measure_element.__init__ (self)
		self._dict = {}
	
	def set_attributes_from_previous (self, dict):
		self._dict.update (dict)
	def read_self (self):
		for c in self.get_all_children ():
			self._dict[c.get_name()] = c

	def get_named_attribute (self, name):
		return self._dict[name]
		
class Note (Measure_element):
	def get_duration_log (self):
		ch = self.get_maybe_exist_typed_child (class_dict[u'type'])

		if ch:
			log = ch.get_text ().strip()
			return 	{'eighth': 3,
				 'quarter': 2,
				 'half': 1,
				 '16th': 4,
				 '32nd': 5,
				 'breve': -1,
				 'long': -2,
				 'whole': 0} [log]
		else:
			return 0
		
	def get_factor (self):
		return 1
	
	def get_pitches (self):
		return self.get_typed_children (class_dict[u'pitch'])


		
class Measure(Music_xml_node):
	def get_notes (self):
		return self.get_typed_children (class_dict[u'note'])

class Part (Music_xml_node):
	def interpret (self):
		"""Set durations and starting points."""
		
		now = Rational (0)
		factor = Rational (1)
		attr_dict = {}
		measures = self.get_typed_children (Measure)

		for m in measures:
			for n in m.get_all_children ():
				dur = Rational (0)
				
				if n.__class__ == Attributes:
					n.set_attributes_from_previous (attr_dict)
					n.read_self ()
					attr_dict = n._dict.copy ()
					
					factor = Rational (1,
							   string.atoi (attr_dict['divisions']
									.get_text ()))
				elif (n.get_maybe_exist_typed_child (Duration)
				      and not n.get_maybe_exist_typed_child (Chord)):
					mxl_dur = n.get_maybe_exist_typed_child (Duration)
					dur = mxl_dur.get_length () * factor
					if n.get_name() == 'backup':
						dur = - dur
					if n.get_maybe_exist_typed_child (Grace):
						dur = Rational (0)
						
				n._when = now
				n._duration = dur
				now += dur

	def extract_voices (part):
		voices = {}
		measures = part.get_typed_children (Measure)
		elements = []
		for m in measures:
			elements.extend (m.get_all_children ())

		start_attr = None
		for n in elements:
			voice_id = n.get_maybe_exist_typed_child (class_dict['voice'])

			if not (voice_id or isinstance (n, Attributes)):
				continue

			if isinstance (n, Attributes) and not start_attr:
				start_attr = n
				continue

			if isinstance (n, Attributes):
				for v in voices.values ():
					v.append (n)
				continue
			
			id = voice_id.get_text ()
			if not voices.has_key (id):
				voices[id] = []

			voices[id].append (n)

		if start_attr:
			for (k,v) in voices.items ():
				v.insert (0, start_attr)
		
		part._voices = voices
	def get_voices (self):
		return self._voices

class Notations (Music_xml_node):
	def get_tuplet (self):
		return self.get_maybe_exist_typed_child (Tuplet)
	def get_slur (self):
		slurs = self.get_typed_children (Slur)

		if not slurs:
			return None
		
		if len (slurs) > 1:
			print "More than one slur?!"
			
		return slurs[0]

class Time_modification(Music_xml_node):
	def get_fraction (self):
		b = self.get_maybe_exist_typed_child (class_dict['actual-notes'])
		a = self.get_maybe_exist_typed_child (class_dict['normal-notes'])
		return (string.atoi(a.get_text ()), string.atoi (b.get_text ()))
		
		
		
class Tuplet(Music_xml_node):
	pass
class Slur (Music_xml_node):
	pass

class Chord (Music_xml_node):
	pass
class Dot (Music_xml_node):
	pass
class Alter (Music_xml_node):
	pass

class Rest (Music_xml_node):
	pass

class Type (Music_xml_node):
	pass
class Grace (Music_xml_node):
	pass

class_dict = {
	'notations': Notations,
	'time-modification': Time_modification,
	'alter': Alter,
	'grace': Grace,
	'rest':Rest,
	'dot': Dot,
	'chord': Chord,
	'duration': Duration,
	'attributes': Attributes,
	'note': Note,
	'pitch': Pitch,
	'part': Part, 
	'measure': Measure,
	'type': Type,
	'slur': Slur,
	'tuplet': Tuplet,
	'#comment': Hash_comment,
}

def name2class_name (name):
	name = name.replace ('-', '_')
	name = name.replace ('#', 'hash_')
	name = name[0].upper() + name[1:].lower()
	
	return str (name)
	
def create_classes (names, dict):
	for n in names:
		if dict.has_key (n):
			continue

		class_name = name2class_name (n)
		klass = new.classobj (class_name, (Music_xml_node,) , {})
		dict[n] = klass
	
def element_names (node, dict):
	dict[node.nodeName] = 1
	for cn in node.childNodes:
		element_names (cn, dict)
	return dict

def demarshal_node (node):
	name = node.nodeName
	klass = class_dict[name]
	py_node = klass()
	py_node._name = name
	py_node._children = [demarshal_node (cn) for cn in node.childNodes]
	for c in py_node._children:
		c._parent = py_node
		
	if node.attributes:
		
		for (nm, value) in node.attributes.items():
			py_node.__dict__[nm] = value

	py_node._data = None
	if node.nodeType == node.TEXT_NODE and node.data:
		py_node._data = node.data 

	py_node._original = node
	return py_node
		
def strip_white_space (node):
	node._children = \
	[c for c in node._children
	 if not (c._original.nodeType == Node.TEXT_NODE and
		 re.match (r'^\s*$', c._data))]
	
	for c in node._children:
		strip_white_space (c)

def create_tree (name):
	doc = minidom.parse(name)
	node = doc.documentElement
	names = element_names (node, {}).keys()
	create_classes (names, class_dict)
    
	return demarshal_node (node)
	
def test_musicxml (tree):
	m = tree._children[-2]
	print m
	
def read_musicxml (name):
	tree = create_tree (name)
	strip_white_space (tree)
	return tree

if __name__  == '__main__':
	tree = read_musicxml ('BeetAnGeSample.xml')
	test_musicxml (tree)


