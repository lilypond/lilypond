import new
from rational import *

class Xml_node:
    def __init__ (self):
	self._children = []
	self._data = None
	self._original = None
	self._name = 'xml_node'
	self._parent = None
        self._attribute_dict = {}
        
    def get_parent (self):
        return self._parent
    
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

    def message (self, msg):
        print msg

        p = self
        while p:
            print '  In: <%s %s>' % (p._name, ' '.join (['%s=%s' % item for item in p._attribute_dict.items()]))
            p = p.get_parent ()
        
    def get_typed_children (self, klass):
	return [c for c in self._children if isinstance(c, klass)]

    def get_named_children (self, nm):
	return self.get_typed_children (class_dict[nm])

    def get_named_child (self, nm):
	return self.get_maybe_exist_named_child (nm)

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
	dur = int (self.get_text ()) * Rational (1,4)
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
	return int (step)

    def get_alteration (self):
	ch = self.get_maybe_exist_typed_child (class_dict[u'alter'])
	alter = 0
	if ch:
	    alter = int (ch.get_text ().strip ())
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

    def get_measure_length (self):
        (n,d) = self.get_time_signature ()
        return Rational (n,d)
        
    def get_time_signature (self):
        "return time sig as a (beat, beat-type) tuple"

        try:
            mxl = self.get_named_attribute ('time')
            
            beats = mxl.get_maybe_exist_named_child ('beats')
            type = mxl.get_maybe_exist_named_child ('beat-type')
            return (int (beats.get_text ()),
                    int (type.get_text ()))
        except KeyError:
            print 'error: requested time signature, but time sig unknown'
            return (4, 4)

    def get_clef_sign (self):
        mxl = self.get_named_attribute ('clef')
        sign = mxl.get_maybe_exist_named_child ('sign')
        if sign:
            return sign.get_text ()
        else:
            print 'clef requested, but unknow'
            return 'G'

    def get_key_signature (self):
        "return (fifths, mode) tuple"

        key = self.get_named_attribute ('key')
        mode_node = key.get_maybe_exist_named_child ('mode')
        mode = 'major'
        if mode_node:
            mode = mode_node.get_text ()

        fifths = int (key.get_maybe_exist_named_child ('fifths').get_text ())
        return (fifths, mode)
                

class Note (Measure_element):
    def __init__ (self):
        Measure_element.__init__ (self)
        self.instrument_name = ''
        
    def get_duration_log (self):
	ch = self.get_maybe_exist_typed_child (class_dict[u'type'])

	if ch:
	    log = ch.get_text ().strip()
	    return {'eighth': 3,
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

class Part_list (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self._id_instrument_name_dict = {}
        
    def generate_id_instrument_dict (self):

        ## not empty to make sure this happens only once.
        mapping = {1: 1}
        for score_part in self.get_named_children ('score-part'):
            for instr in score_part.get_named_children ('score-instrument'):
                id = instr.id
                name = instr.get_named_child ("instrument-name")
                mapping[id] = name.get_text ()

        self._id_instrument_name_dict = mapping

    def get_instrument (self, id):
        if not self._id_instrument_name_dict:
            self.generate_id_instrument_dict()

        try:
            return self._id_instrument_name_dict[id]
        except KeyError:
            print "Opps, couldn't find instrument for ID=", id
            return "Grand Piano"
        
class Measure(Music_xml_node):
    def get_notes (self):
	return self.get_typed_children (class_dict[u'note'])

    
class Musicxml_voice:
    def __init__ (self):
	self._elements = []
	self._staves = {}
	self._start_staff = None

    def add_element (self, e):
	self._elements.append (e)
	if (isinstance (e, Note)
	    and e.get_maybe_exist_typed_child (Staff)):
	    name = e.get_maybe_exist_typed_child (Staff).get_text ()

	    if not self._start_staff:
		self._start_staff = name
	    self._staves[name] = True

    def insert (self, idx, e):
	self._elements.insert (idx, e)



class Part (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
	self._voices = []

    def get_part_list (self):
        n = self
        while n and n.get_name() != 'score-partwise':
            n = n._parent

        return n.get_named_child ('part-list')
        
    def interpret (self):
	"""Set durations and starting points."""
        
        part_list = self.get_part_list ()
        
	now = Rational (0)
	factor = Rational (1)
	attributes_dict = {}
        attributes_object = None
	measures = self.get_typed_children (Measure)
        last_moment = Rational (-1)
        last_measure_position = Rational (-1)
	for m in measures:
            measure_start_moment = now
            measure_position = Rational (0)
	    for n in m.get_all_children ():
		dur = Rational (0)

                if n.__class__ == Attributes:
		    n.set_attributes_from_previous (attributes_dict)
		    n.read_self ()
		    attributes_dict = n._dict.copy ()
                    attributes_object = n
                    
		    factor = Rational (1,
				       int (attributes_dict['divisions'].get_text ()))

                
		if (n.get_maybe_exist_typed_child (Duration)):
		    mxl_dur = n.get_maybe_exist_typed_child (Duration)
		    dur = mxl_dur.get_length () * factor
                    
		    if n.get_name() == 'backup':
			dur = - dur
		    if n.get_maybe_exist_typed_child (Grace):
			dur = Rational (0)

                    rest = n.get_maybe_exist_typed_child (Rest)
		    if (rest
                        and attributes_object
                        and attributes_object.get_measure_length () == dur):

                        rest._is_whole_measure = True

                if (dur > Rational (0) 
                    and n.get_maybe_exist_typed_child (Chord)):
                    now = last_moment
                    measure_position = last_measure_position
                    
                last_moment = now
                last_measure_position = measure_position

		n._when = now
                n._measure_position = measure_position
		n._duration = dur
		now += dur
                measure_position += dur
                if n._name == 'note':
                    instrument = n.get_maybe_exist_named_child ('instrument')
                    if instrument:
                        n.instrument_name = part_list.get_instrument (instrument.id)

            if attributes_object:
                length = attributes_object.get_measure_length ()
                new_now = measure_start_moment + length
                
                if now <> new_now:
                    problem = 'incomplete'
                    if now > new_now:
                        problem = 'overfull'

                    ## only for verbose operation.
                    if problem <> 'incomplete':
                        m.message ('%s measure? Expected: %s, Difference: %s' % (problem, now, new_now - now))

                now = new_now

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
		    v.add_element (n)
		continue

	    id = voice_id.get_text ()
	    if not voices.has_key (id):
		voices[id] = Musicxml_voice()

	    voices[id].add_element (n)

	if start_attr:
	    for (k,v) in voices.items ():
		v.insert (0, start_attr)

	part._voices = voices

    def get_voices (self):
	return self._voices

class Notations (Music_xml_node):
    def get_tie (self):
	ts = self.get_named_children ('tied')
	starts = [t for t in ts if t.type == 'start']
	if starts:
	    return starts[0]
	else:
	    return None

    def get_tuplet (self):
	return self.get_maybe_exist_typed_child (Tuplet)

class Time_modification(Music_xml_node):
    def get_fraction (self):
	b = self.get_maybe_exist_typed_child (class_dict['actual-notes'])
	a = self.get_maybe_exist_typed_child (class_dict['normal-notes'])
	return (int(a.get_text ()), int (b.get_text ()))

class Accidental (Music_xml_node):
    def __init__ (self):
	Music_xml_node.__init__ (self)
	self.editorial = False
	self.cautionary = False


class Tuplet(Music_xml_node):
    pass

class Slur (Music_xml_node):
    def get_type (self):
	return self.type

class Beam (Music_xml_node):
    def get_type (self):
	return self.get_text ()
    def is_primary (self):
        return self.number == "1"
    
class Chord (Music_xml_node):
    pass

class Dot (Music_xml_node):
    pass

class Alter (Music_xml_node):
    pass

class Rest (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self._is_whole_measure = False
    def is_whole_measure (self):
        return self._is_whole_measure

class Mode (Music_xml_node):
    pass
class Tied (Music_xml_node):
    pass

class Type (Music_xml_node):
    pass
class Grace (Music_xml_node):
    pass
class Staff (Music_xml_node):
    pass

class Instrument (Music_xml_node):
    pass

class Fermata (Music_xml_node):
    pass
class Dynamics (Music_xml_node):
    pass
class Articulations (Music_xml_node):
    pass
class Accent (Music_xml_node):
    pass
class Staccato (Music_xml_node):
    pass
class Tenuto (Music_xml_node):
    pass
class Tremolo (Music_xml_node):
    pass
class Technical (Music_xml_node):
    pass
class Ornaments (Music_xml_node):
    pass


class Direction (Music_xml_node):
    pass
class DirType (Music_xml_node):
    pass


## need this, not all classes are instantiated
## for every input file.
class_dict = {
	'#comment': Hash_comment,
	'accidental': Accidental,
	'alter': Alter,
	'attributes': Attributes,
	'beam' : Beam,
	'chord': Chord,
	'dot': Dot,
	'duration': Duration,
	'grace': Grace,
        'instrument': Instrument, 
	'mode' : Mode,
	'measure': Measure,
	'notations': Notations,
	'note': Note,
	'part': Part,
	'pitch': Pitch,
	'rest':Rest,
	'slur': Slur,
	'tied': Tied,
	'time-modification': Time_modification,
	'tuplet': Tuplet,
	'type': Type,
	'part-list': Part_list,
	'staff': Staff,
        'fermata': Fermata,
        'articulations': Articulations,
        'accent': Accent,
        'staccato': Staccato,
        'tenuto': Tenuto,
        'tremolo': Tremolo,
        'technical': Technical,
        'ornaments': Ornaments,
        'direction': Direction,
        'direction-type': DirType
}

def name2class_name (name):
    name = name.replace ('-', '_')
    name = name.replace ('#', 'hash_')
    name = name[0].upper() + name[1:].lower()

    return str (name)

def get_class (name):
    try:
        return class_dict[name]
    except KeyError:
	class_name = name2class_name (name)
	klass = new.classobj (class_name, (Music_xml_node,) , {})
	class_dict[name] = klass
        return klass
        
def lxml_demarshal_node (node):
    name = node.tag

    if name is None:
        return None
    klass = get_class (name)
    py_node = klass()
    
    py_node._original = node
    py_node._name = name
    py_node._data = node.text
    py_node._children = [lxml_demarshal_node (cn) for cn in node.getchildren()]
    py_node._children = filter (lambda x: x, py_node._children)
    
    for c in py_node._children:
	c._parent = py_node

    for (k,v) in node.items ():
        py_node.__dict__[k] = v
        py_node._attribute_dict[k] = v

    return py_node

def minidom_demarshal_node (node):
    name = node.nodeName

    klass = get_class (name)
    py_node = klass()
    py_node._name = name
    py_node._children = [minidom_demarshal_node (cn) for cn in node.childNodes]
    for c in py_node._children:
	c._parent = py_node

    if node.attributes:
	for (nm, value) in node.attributes.items():
	    py_node.__dict__[nm] = value
            py_node._attribute_dict[nm] = value
            
    py_node._data = None
    if node.nodeType == node.TEXT_NODE and node.data:
	py_node._data = node.data 

    py_node._original = node
    return py_node


if __name__  == '__main__':
        import lxml.etree
        
        tree = lxml.etree.parse ('beethoven.xml')
        mxl_tree = lxml_demarshal_node (tree.getroot ())
        ks = class_dict.keys()
        ks.sort()
        print '\n'.join (ks)
