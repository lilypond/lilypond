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
		return [c for c in self._children if c.__class__ == klass]

	def get_children (self, predicate):
		return [c for c in self._children if predicate(c)]

	def get_all_children (self):
		return self._children

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
	
class Attributes (Music_xml_node):
	def __init__ (self):
		self._dict = {}
		
	def set_attributes_from_previous (self, dict):
		self._dict.update (dict)
	def read_self (self):
		for c in self.get_all_children ():
			self._dict[c.get_name()] = c

	def get_named_attribute (self, name):
		return self._dict[name]
		
class Duration (Music_xml_node):
	def get_length (self):
		dur = string.atoi (self.get_text ()) * Rational (1,4)
		return dur
		
class Hash_comment (Music_xml_node):
	def to_ly (self, output_func):
		output_func ('%% %s\n ' % self.get_text())
		
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

	def to_ly (self, output_func):
		oct = (self.get_octave () - 4)
		oct_str = ''
		if oct > 0:
			oct_str = "'" * oct
		elif oct < 0:
			oct_str = "," * -oct

		alt = self.get_alteration ()
		alt_str = ''
		if alt > 0:
			alt_str = 'is' * alt
		elif alt < 0:
			alt_str = 'es' * alt 
		
		output_func ('%s%s%s' % (self.get_step ().lower(), alt_str, oct_str))
				       
class Note (Music_xml_node):
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

	def to_ly (self, func):
		ps = self.get_pitches ()

		if len (ps) == 0:
			func ('r')
		else:
			func ('<')
			for p in ps:
				p.to_ly (func)
			func ('>')
		
		func ('%d ' % (1 << self.get_duration_log ()))
		


		
class Measure(Music_xml_node):
	def get_notes (self):
		return self.get_typed_children (class_dict[u'note'])
	def to_ly (self, func):
		func (' { % measure \n ')
		for c in self._children:
			c.to_ly (func)
		func (' } \n ')

class Part (Music_xml_node):
	def to_ly (self, func):
		func (' { %% part %s \n ' % self.name)
		for c in self._children:
			c.to_ly (func)
		func (' } \n ')

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
			elements.extend (m.get_typed_children (Note))

		for n in elements:
			voice_id = n.get_maybe_exist_typed_child (class_dict['voice'])

			if not voice_id:
				continue
			
			id = voice_id.get_text ()
			if not voices.has_key (id):
				voices[id] = []

			voices[id].append (n)
			
		part._voices = voices
	def get_voices (self):
		return self._voices
	
class Chord (Music_xml_node):
	pass

class Dot (Music_xml_node):
	pass

class Rest (Music_xml_node):
	pass

class Type (Music_xml_node):
	pass
class Grace (Music_xml_node):
	pass

class_dict = {
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
	if node.attributes:
		for (name, value) in node.attributes.items():
			py_node.name = value

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

def oldtest ():
	n = tree._children[-2]._children[-1]._children[0]
	print n
	print n.get_duration_log()
	print n.get_pitches()
	print n.get_pitches()[0].get_alteration()
	
	
def test_musicxml (tree):
	m = tree._children[-2]
	print m
	
	m.to_ly (lambda str: sys.stdout.write (str))
	
def read_musicxml (name):
	tree = create_tree (name)
	strip_white_space (tree)
	return tree






if __name__  == '__main__':
	tree = read_musicxml ('BeetAnGeSample.xml')
	test_musicxml (tree)

