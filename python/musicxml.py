# -*- coding: utf-8 -*-
import new
import string
from rational import *
import re
import sys
import copy
import lilylib as ly

_ = ly._


def escape_ly_output_string (input_string):
    return_string = input_string
    needs_quotes = not re.match (u"^[a-zA-ZäöüÜÄÖßñ]*$", return_string);
    if needs_quotes:
        return_string = "\"" + string.replace (return_string, "\"", "\\\"") + "\""
    return return_string


def musicxml_duration_to_log (dur):
    return  {'256th': 8,
             '128th': 7,
             '64th': 6,
             '32nd': 5,
             '16th': 4,
             'eighth': 3,
             'quarter': 2,
             'half': 1,
             'whole': 0,
             'breve': -1,
             'longa': -2,
             'long': -2}.get (dur, 0)



def interpret_alter_element (alter_elm):
    alter = 0
    if alter_elm:
        val = eval(alter_elm.get_text ())
        if type (val) in (int, float):
            alter = val
    return alter


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
        ly.warning (msg)

        p = self
        while p:
            ly.progress ('  In: <%s %s>\n' % (p._name, ' '.join (['%s=%s' % item for item in p._attribute_dict.items ()])))
            p = p.get_parent ()

    def dump (self, indent = ''):
        ly.debug_output ('%s<%s%s>' % (indent, self._name, ''.join ([' %s=%s' % item for item in self._attribute_dict.items ()])))
        non_text_children = [c for c in self._children if not isinstance (c, Hash_text)]
        if non_text_children:
            ly.debug_output ('\n')
        for c in self._children:
            c.dump (indent + "    ")
        if non_text_children:
            ly.debug_output (indent)
        ly.debug_output ('</%s>\n' % self._name)


    def get_typed_children (self, klass):
        if not klass:
            return []
        else:
            return [c for c in self._children if isinstance(c, klass)]

    def get_named_children (self, nm):
        return self.get_typed_children (get_class (nm))

    def get_named_child (self, nm):
        return self.get_maybe_exist_named_child (nm)

    def get_children (self, predicate):
        return [c for c in self._children if predicate(c)]

    def get_all_children (self):
        return self._children

    def get_maybe_exist_named_child (self, name):
        return self.get_maybe_exist_typed_child (get_class (name))

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
            ly.error (self.__dict__)
            raise 'Child is not unique for', (klass, 'found', cn)

        return cn[0]

    def get_named_child_value_number (self, name, default):
        n = self.get_maybe_exist_named_child (name)
        if n:
            return string.atoi (n.get_text())
        else:
            return default


class Music_xml_node (Xml_node):
    def __init__ (self):
        Xml_node.__init__ (self)
        self.duration = Rational (0)
        self.start = Rational (0)

class Work (Xml_node):
    def get_work_information (self, tag):
        wt = self.get_maybe_exist_named_child (tag)
        if wt:
            return wt.get_text ()
        else:
            return ''

    def get_work_title (self):
        return self.get_work_information ('work-title')
    def get_work_number (self):
        return self.get_work_information ('work-number')
    def get_opus (self):
        return self.get_work_information ('opus')

class Identification (Xml_node):
    def get_rights (self):
        rights = self.get_named_children ('rights')
        ret = []
        for r in rights:
          ret.append (r.get_text ())
        return string.join (ret, "\n")

    # get contents of the source-element (usually used for publishing information). (These contents are saved in a custom variable named "source" in the header of the .ly file.)
    def get_source (self):
        source = self.get_named_children ('source')
        ret = []
        for r in source:
          ret.append (r.get_text ())
        return string.join (ret, "\n")

    def get_creator (self, type):
        creators = self.get_named_children ('creator')
        # return the first creator tag that has the particular type
        for i in creators:
            if hasattr (i, 'type') and i.type == type:
                return i.get_text ()
        return None

    def get_composer (self):
        c = self.get_creator ('composer')
        if c:
            return c
        creators = self.get_named_children ('creator')
        # return the first creator tag that has no type at all
        for i in creators:
            if not hasattr (i, 'type'):
                return i.get_text ()
        return None
    def get_arranger (self):
        return self.get_creator ('arranger')
    def get_editor (self):
        return self.get_creator ('editor')
    def get_poet (self):
        v = self.get_creator ('lyricist')
        if v:
            return v
        v = self.get_creator ('poet')
        return v

    def get_encoding_information (self, type):
        enc = self.get_named_children ('encoding')
        if enc:
            children = enc[0].get_named_children (type)
            if children:
                return children[0].get_text ()
        else:
            return None

    def get_encoding_software (self):
        return self.get_encoding_information ('software')
    def get_encoding_date (self):
        return self.get_encoding_information ('encoding-date')
    def get_encoding_person (self):
        return self.get_encoding_information ('encoder')
    def get_encoding_description (self):
        return self.get_encoding_information ('encoding-description')

    def get_encoding_software_list (self):
        enc = self.get_named_children ('encoding')
        software = []
        for e in enc:
            softwares = e.get_named_children ('software')
            for s in softwares:
                software.append (s.get_text ())
        return software

    def get_file_description (self):
        misc = self.get_named_children ('miscellaneous')
        for m in misc:
            misc_fields = m.get_named_children ('miscellaneous-field')
            for mf in misc_fields:
                if hasattr (mf, 'name') and mf.name == 'description':
                    return mf.get_text ()
        return None

class Duration (Music_xml_node):
    def get_length (self):
        dur = int (self.get_text ()) * Rational (1,4)
        return dur

class Hash_comment (Music_xml_node):
    pass
class Hash_text (Music_xml_node):
    def dump (self, indent = ''):
        ly.debug_output ('%s' % string.strip (self._data))

class Pitch (Music_xml_node):
    def get_step (self):
        ch = self.get_unique_typed_child (get_class (u'step'))
        step = ch.get_text ().strip ()
        return step
    def get_octave (self):
        ch = self.get_unique_typed_child (get_class (u'octave'))
        octave = ch.get_text ().strip ()
        return int (octave)

    def get_alteration (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'alter'))
        return interpret_alter_element (ch)

class Unpitched (Music_xml_node):
    def get_step (self):
        ch = self.get_unique_typed_child (get_class (u'display-step'))
        step = ch.get_text ().strip ()
        return step

    def get_octave (self):
        ch = self.get_unique_typed_child (get_class (u'display-octave'))

        if ch:
            octave = ch.get_text ().strip ()
            return int (octave)
        else:
            return None

class Measure_element (Music_xml_node):
    def get_voice_id (self):
        voice_id = self.get_maybe_exist_named_child ('voice')
        if voice_id:
            return voice_id.get_text ()
        else:
            return None

    def is_first (self):
        # Look at all measure elements (previously we had self.__class__, which
        # only looked at objects of the same type!
        cn = self._parent.get_typed_children (Measure_element)
        # But only look at the correct voice; But include Attributes, too, which
        # are not tied to any particular voice
        cn = [c for c in cn if (c.get_voice_id () == self.get_voice_id ()) or isinstance (c, Attributes)]
        return cn[0] == self

class Attributes (Measure_element):
    def __init__ (self):
        Measure_element.__init__ (self)
        self._dict = {}
        self._original_tag = None
        self._time_signature_cache = None

    def is_first (self):
        cn = self._parent.get_typed_children (self.__class__)
        if self._original_tag:
            return cn[0] == self._original_tag
        else:
            return cn[0] == self

    def set_attributes_from_previous (self, dict):
        self._dict.update (dict)

    def read_self (self):
        for c in self.get_all_children ():
            self._dict[c.get_name()] = c

    def get_named_attribute (self, name):
        return self._dict.get (name)

    def single_time_sig_to_fraction (self, sig):
        if len (sig) < 2:
            return 0
        n = 0
        for i in sig[0:-1]:
          n += i
        return Rational (n, sig[-1])

    def get_measure_length (self):
        sig = self.get_time_signature ()
        if not sig or len (sig) == 0:
            return 1
        if isinstance (sig[0], list):
            # Complex compound time signature
            l = 0
            for i in sig:
                l += self.single_time_sig_to_fraction (i)
            return l
        else:
           # Simple (maybe compound) time signature of the form (beat, ..., type)
            return self.single_time_sig_to_fraction (sig)
        return 0

    def get_time_signature (self):
        "Return time sig as a (beat, beat-type) tuple. For compound signatures,"
        "return either (beat, beat,..., beat-type) or ((beat,..., type), "
        "(beat,..., type), ...)."
        if self._time_signature_cache:
            return self._time_signature_cache

        try:
            mxl = self.get_named_attribute ('time')
            if not mxl:
                return None

            if mxl.get_maybe_exist_named_child ('senza-misura'):
                # TODO: Handle pieces without a time signature!
                ly.warning (_ ("Senza-misura time signatures are not yet supported!"))
                return (4, 4)
            else:
                signature = []
                current_sig = []
                for i in mxl.get_all_children ():
                    if isinstance (i, Beats):
                        beats = string.split (i.get_text ().strip (), "+")
                        current_sig = [int (j) for j in beats]
                    elif isinstance (i, BeatType):
                        current_sig.append (int (i.get_text ()))
                        signature.append (current_sig)
                        current_sig = []
                if isinstance (signature[0], list) and len (signature) == 1:
                    signature = signature[0]
                self._time_signature_cache = signature
                return signature
        except (KeyError, ValueError):
            self.message (_ ("Unable to interpret time signature! Falling back to 4/4."))
            return (4, 4)

    # returns clef information in the form ("cleftype", position, octave-shift)
    def get_clef_information (self):
        clefinfo = ['G', 2, 0]
        mxl = self.get_named_attribute ('clef')
        if not mxl:
            return clefinfo
        sign = mxl.get_maybe_exist_named_child ('sign')
        if sign:
            clefinfo[0] = sign.get_text()
        line = mxl.get_maybe_exist_named_child ('line')
        if line:
            clefinfo[1] = string.atoi (line.get_text ())
        octave = mxl.get_maybe_exist_named_child ('clef-octave-change')
        if octave:
            clefinfo[2] = string.atoi (octave.get_text ())
        return clefinfo

    def get_key_signature (self):
        "return (fifths, mode) tuple if the key signatures is given as "
        "major/minor in the Circle of fifths. Otherwise return an alterations"
        "list of the form [[step,alter<,octave>], [step,alter<,octave>], ...], "
        "where the octave values are optional."

        key = self.get_named_attribute ('key')
        if not key:
            return None
        fifths_elm = key.get_maybe_exist_named_child ('fifths')
        if fifths_elm:
            mode_node = key.get_maybe_exist_named_child ('mode')
            mode = None
            if mode_node:
                mode = mode_node.get_text ()
            if not mode or mode == '':
                mode = 'major'
            fifths = int (fifths_elm.get_text ())
            # TODO: Shall we try to convert the key-octave and the cancel, too?
            return (fifths, mode)
        else:
            alterations = []
            current_step = 0
            for i in key.get_all_children ():
                if isinstance (i, KeyStep):
                    current_step = i.get_text ().strip ()
                elif isinstance (i, KeyAlter):
                    alterations.append ([current_step, interpret_alter_element (i)])
                elif isinstance (i, KeyOctave):
                    nr = -1
                    if hasattr (i, 'number'):
                        nr = int (i.number)
                    if (nr > 0) and (nr <= len (alterations)):
                        # MusicXML Octave 4 is middle C -> shift to 0
                        alterations[nr-1].append (int (i.get_text ())-4)
                    else:
                        i.message (_ ("Key alteration octave given for a "
                            "non-existing alteration nr. %s, available numbers: %s!") % (nr, len(alterations)))
            return alterations

    def get_transposition (self):
        return self.get_named_attribute ('transpose')

class KeyAlter (Music_xml_node):
    pass
class KeyStep (Music_xml_node):
    pass
class KeyOctave (Music_xml_node):
    pass


class Barline (Measure_element):
    pass
class BarStyle (Music_xml_node):
    pass
class Partial (Measure_element):
    def __init__ (self, partial):
        Measure_element.__init__ (self)
        self.partial = partial

class Note (Measure_element):
    def __init__ (self):
        Measure_element.__init__ (self)
        self.instrument_name = ''
        self._after_grace = False
    def is_grace (self):
        return self.get_maybe_exist_named_child (u'grace')
    def is_after_grace (self):
        if not self.is_grace():
            return False;
        gr = self.get_maybe_exist_typed_child (Grace)
        return self._after_grace or hasattr (gr, 'steal-time-previous');

    def get_duration_log (self):
        ch = self.get_maybe_exist_named_child (u'type')

        if ch:
            log = ch.get_text ().strip()
            return musicxml_duration_to_log (log)
        elif self.get_maybe_exist_named_child (u'grace'):
            # FIXME: is it ok to default to eight note for grace notes?
            return 3
        else:
            return None

    def get_duration_info (self):
        log = self.get_duration_log ()
        if log != None:
            dots = len (self.get_typed_children (Dot))
            return (log, dots)
        else:
            return None

    def get_factor (self):
        return 1

    def get_pitches (self):
        return self.get_typed_children (get_class (u'pitch'))

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

        instrument_name = self._id_instrument_name_dict.get (id)
        if instrument_name:
            return instrument_name
        else:
            ly.warning (_ ("Unable to find instrument for ID=%s\n") % id)
            return "Grand Piano"

class Part_group (Music_xml_node):
    pass
class Score_part (Music_xml_node):
    pass

class Measure (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self.partial = 0
    def is_implicit (self):
        return hasattr (self, 'implicit') and self.implicit == 'yes'
    def get_notes (self):
        return self.get_typed_children (get_class (u'note'))

class Syllabic (Music_xml_node):
    def continued (self):
        text = self.get_text()
        return (text == "begin") or (text == "middle")
class Elision (Music_xml_node):
    pass
class Extend (Music_xml_node):
    pass
class Text (Music_xml_node):
    pass

class Lyric (Music_xml_node):
    def get_number (self):
        if hasattr (self, 'number'):
            return self.number
        else:
            return -1

class Musicxml_voice:
    def __init__ (self):
        self._elements = []
        self._staves = {}
        self._start_staff = None
        self._lyrics = []
        self._has_lyrics = False

    def add_element (self, e):
        self._elements.append (e)
        if (isinstance (e, Note)
            and e.get_maybe_exist_typed_child (Staff)):
            name = e.get_maybe_exist_typed_child (Staff).get_text ()

            if not self._start_staff and not e.get_maybe_exist_typed_child (Grace):
                self._start_staff = name
            self._staves[name] = True

        lyrics = e.get_typed_children (Lyric)
        if not self._has_lyrics:
          self.has_lyrics = len (lyrics) > 0

        for l in lyrics:
            nr = l.get_number()
            if (nr > 0) and not (nr in self._lyrics):
                self._lyrics.append (nr)

    def insert (self, idx, e):
        self._elements.insert (idx, e)

    def get_lyrics_numbers (self):
        if (len (self._lyrics) == 0) and self._has_lyrics:
            #only happens if none of the <lyric> tags has a number attribute
            return [1]
        else:
            return self._lyrics


def graces_to_aftergraces (pending_graces):
    for gr in pending_graces:
        gr._when = gr._prev_when
        gr._measure_position = gr._prev_measure_position
        gr._after_grace = True


class Part (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self._voices = {}
        self._staff_attributes_dict = {}

    def get_part_list (self):
        n = self
        while n and n.get_name() != 'score-partwise':
            n = n._parent

        return n.get_named_child ('part-list')

    def interpret (self):
        """Set durations and starting points."""
        """The starting point of the very first note is 0!"""

        part_list = self.get_part_list ()

        now = Rational (0)
        factor = Rational (1)
        attributes_dict = {}
        attributes_object = None
        measures = self.get_typed_children (Measure)
        last_moment = Rational (-1)
        last_measure_position = Rational (-1)
        measure_position = Rational (0)
        measure_start_moment = now
        is_first_measure = True
        previous_measure = None
        # Graces at the end of a measure need to have their position set to the
        # previous number!
        pending_graces = []
        for m in measures:
            # implicit measures are used for artificial measures, e.g. when
            # a repeat bar line splits a bar into two halves. In this case,
            # don't reset the measure position to 0. They are also used for
            # upbeats (initial value of 0 fits these, too).
            # Also, don't reset the measure position at the end of the loop,
            # but rather when starting the next measure (since only then do we
            # know if the next measure is implicit and continues that measure)
            if not m.is_implicit ():
                # Warn about possibly overfull measures and reset the position
                if attributes_object and previous_measure and previous_measure.partial == 0:
                    length = attributes_object.get_measure_length ()
                    new_now = measure_start_moment + length
                    if now <> new_now:
                        problem = 'incomplete'
                        if now > new_now:
                            problem = 'overfull'
                        ## only for verbose operation.
                        if problem <> 'incomplete' and previous_measure:
                            previous_measure.message ('%s measure? Expected: %s, Difference: %s' % (problem, now, new_now - now))
                    now = new_now
                measure_start_moment = now
                measure_position = Rational (0)

            for n in m.get_all_children ():
                # figured bass has a duration, but applies to the next note
                # and should not change the current measure position!
                if isinstance (n, FiguredBass):
                    n._divisions = factor.denominator ()
                    n._when = now
                    n._measure_position = measure_position
                    continue

                if isinstance (n, Hash_text):
                    continue
                dur = Rational (0)

                if n.__class__ == Attributes:
                    n.set_attributes_from_previous (attributes_dict)
                    n.read_self ()
                    attributes_dict = n._dict.copy ()
                    attributes_object = n

                    factor = Rational (1,
                                       int (attributes_dict.get ('divisions').get_text ()))


                if (n.get_maybe_exist_typed_child (Duration)):
                    mxl_dur = n.get_maybe_exist_typed_child (Duration)
                    dur = mxl_dur.get_length () * factor

                    if n.get_name() == 'backup':
                        dur = - dur
                        # reset all graces before the backup to after-graces:
                        graces_to_aftergraces (pending_graces)
                        pending_graces = []
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

                n._when = now
                n._measure_position = measure_position

                # For all grace notes, store the previous note,  in case need
                # to turn the grace note into an after-grace later on!
                if isinstance(n, Note) and n.is_grace ():
                    n._prev_when = last_moment
                    n._prev_measure_position = last_measure_position
                # After-graces are placed at the same position as the previous note
                if isinstance(n, Note) and  n.is_after_grace ():
                    # TODO: We should do the same for grace notes at the end of
                    # a measure with no following note!!!
                    n._when = last_moment
                    n._measure_position = last_measure_position
                elif isinstance(n, Note) and n.is_grace ():
                    pending_graces.append (n)
                elif (dur > Rational (0)):
                    pending_graces = [];

                n._duration = dur
                if dur > Rational (0):
                    last_moment = now
                    last_measure_position = measure_position
                    now += dur
                    measure_position += dur
                elif dur < Rational (0):
                    # backup element, reset measure position
                    now += dur
                    measure_position += dur
                    if measure_position < 0:
                        # backup went beyond the measure start => reset to 0
                        now -= measure_position
                        measure_position = 0
                    last_moment = now
                    last_measure_position = measure_position
                if n._name == 'note':
                    instrument = n.get_maybe_exist_named_child ('instrument')
                    if instrument:
                        n.instrument_name = part_list.get_instrument (instrument.id)

            # reset all graces at the end of the measure to after-graces:
            graces_to_aftergraces (pending_graces)
            pending_graces = []
            # Incomplete first measures are not padded, but registered as partial
            if is_first_measure:
                is_first_measure = False
                # upbeats are marked as implicit measures
                if attributes_object and m.is_implicit ():
                    length = attributes_object.get_measure_length ()
                    measure_end = measure_start_moment + length
                    if measure_end <> now:
                        m.partial = now
            previous_measure = m

    # modify attributes so that only those applying to the given staff remain
    def extract_attributes_for_staff (part, attr, staff):
        attributes = copy.copy (attr)
        attributes._children = [];
        attributes._dict = attr._dict.copy ()
        attributes._original_tag = attr
        # copy only the relevant children over for the given staff
        if staff == "None":
            staff = "1"
        for c in attr._children:
            if (not (hasattr (c, 'number') and (c.number != staff)) and
                not (isinstance (c, Hash_text))):
                attributes._children.append (c)
        if not attributes._children:
            return None
        else:
            return attributes

    def extract_voices (part):
        voices = {}
        measures = part.get_typed_children (Measure)
        elements = []
        for m in measures:
            if m.partial > 0:
                elements.append (Partial (m.partial))
            elements.extend (m.get_all_children ())
        # make sure we know all voices already so that dynamics, clefs, etc.
        # can be assigned to the correct voices
        voice_to_staff_dict = {}
        for n in elements:
            voice_id = n.get_maybe_exist_named_child (u'voice')
            vid = None
            if voice_id:
                vid = voice_id.get_text ()
            elif isinstance (n, Note):
                # TODO: Check whether we shall really use "None" here, or
                #       rather use "1" as the default?
                vid = "None"

            staff_id = n.get_maybe_exist_named_child (u'staff')
            sid = None
            if staff_id:
                sid = staff_id.get_text ()
            else:
                # TODO: Check whether we shall really use "None" here, or
                #       rather use "1" as the default?
                #       If this is changed, need to change the corresponding
                #       check in extract_attributes_for_staff, too.
                sid = "None"
            if vid and not voices.has_key (vid):
                voices[vid] = Musicxml_voice()
            if vid and sid and not n.get_maybe_exist_typed_child (Grace):
                if not voice_to_staff_dict.has_key (vid):
                    voice_to_staff_dict[vid] = sid
        # invert the voice_to_staff_dict into a staff_to_voice_dict (since we
        # need to assign staff-assigned objects like clefs, times, etc. to
        # all the correct voices. This will never work entirely correct due
        # to staff-switches, but that's the best we can do!
        staff_to_voice_dict = {}
        for (v,s) in voice_to_staff_dict.items ():
            if not staff_to_voice_dict.has_key (s):
                staff_to_voice_dict[s] = [v]
            else:
                staff_to_voice_dict[s].append (v)


        start_attr = None
        assign_to_next_note = []
        id = None
        for n in elements:
            voice_id = n.get_maybe_exist_typed_child (get_class ('voice'))
            if voice_id:
                id = voice_id.get_text ()
            else:
                id = "None"

            # We don't need backup/forward any more, since we have already
            # assigned the correct onset times.
            # TODO: Let Grouping through. Also: link, print, bokmark sound
            if not (isinstance (n, Note) or isinstance (n, Attributes) or
                    isinstance (n, Direction) or isinstance (n, Partial) or
                    isinstance (n, Barline) or isinstance (n, Harmony) or
                    isinstance (n, FiguredBass) or isinstance (n, Print)):
                continue

            if isinstance (n, Attributes) and not start_attr:
                start_attr = n
                continue

            if isinstance (n, Attributes):
                # assign these only to the voices they really belong to!
                for (s, vids) in staff_to_voice_dict.items ():
                    staff_attributes = part.extract_attributes_for_staff (n, s)
                    if staff_attributes:
                        for v in vids:
                            voices[v].add_element (staff_attributes)
                continue

            if isinstance (n, Partial) or isinstance (n, Barline) or isinstance (n, Print):
                for v in voices.keys ():
                    voices[v].add_element (n)
                continue

            if isinstance (n, Direction):
                staff_id = n.get_maybe_exist_named_child (u'staff')
                if staff_id:
                    staff_id = staff_id.get_text ()
                if staff_id:
                    dir_voices = staff_to_voice_dict.get (staff_id, voices.keys ())
                else:
                    dir_voices = voices.keys ()
                for v in dir_voices:
                    voices[v].add_element (n)
                continue

            if isinstance (n, Harmony) or isinstance (n, FiguredBass):
                # store the harmony or figured bass element until we encounter
                # the next note and assign it only to that one voice.
                assign_to_next_note.append (n)
                continue

            if hasattr (n, 'print-object') and getattr (n, 'print-object') == "no":
                #Skip this note.
                pass
            else:
                for i in assign_to_next_note:
                    voices[id].add_element (i)
                assign_to_next_note = []
                voices[id].add_element (n)

        # Assign all remaining elements from assign_to_next_note to the voice
        # of the previous note:
        for i in assign_to_next_note:
            voices[id].add_element (i)
        assign_to_next_note = []

        if start_attr:
            for (s, vids) in staff_to_voice_dict.items ():
                staff_attributes = part.extract_attributes_for_staff (start_attr, s)
                staff_attributes.read_self ()
                part._staff_attributes_dict[s] = staff_attributes
                for v in vids:
                    voices[v].insert (0, staff_attributes)
                    voices[v]._elements[0].read_self()

        part._voices = voices

    def get_voices (self):
        return self._voices
    def get_staff_attributes (self):
        return self._staff_attributes_dict

class Notations (Music_xml_node):
    def get_tie (self):
        ts = self.get_named_children ('tied')
        starts = [t for t in ts if t.type == 'start']
        if starts:
            return starts[0]
        else:
            return None

    def get_tuplets (self):
        return self.get_typed_children (Tuplet)

class Time_modification(Music_xml_node):
    def get_fraction (self):
        b = self.get_maybe_exist_named_child ('actual-notes')
        a = self.get_maybe_exist_named_child ('normal-notes')
        return (int(a.get_text ()), int (b.get_text ()))

    def get_normal_type (self):
        tuplet_type = self.get_maybe_exist_named_child ('normal-type')
        if tuplet_type:
            dots = self.get_named_children ('normal-dot')
            log = musicxml_duration_to_log (tuplet_type.get_text ().strip ())
            return (log , len (dots))
        else:
            return None


class Accidental (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self.editorial = False
        self.cautionary = False

class Music_xml_spanner (Music_xml_node):
    def get_type (self):
        if hasattr (self, 'type'):
            return self.type
        else:
            return 0
    def get_size (self):
        if hasattr (self, 'size'):
            return string.atoi (self.size)
        else:
            return 0

class Wedge (Music_xml_spanner):
    pass

class Tuplet (Music_xml_spanner):
    def duration_info_from_tuplet_note (self, tuplet_note):
        tuplet_type = tuplet_note.get_maybe_exist_named_child ('tuplet-type')
        if tuplet_type:
            dots = tuplet_note.get_named_children ('tuplet-dot')
            log = musicxml_duration_to_log (tuplet_type.get_text ().strip ())
            return (log, len (dots))
        else:
            return None

    # Return tuplet note type as (log, dots)
    def get_normal_type (self):
        tuplet = self.get_maybe_exist_named_child ('tuplet-normal')
        if tuplet:
            return self.duration_info_from_tuplet_note (tuplet)
        else:
            return None

    def get_actual_type (self):
        tuplet = self.get_maybe_exist_named_child ('tuplet-actual')
        if tuplet:
            return self.duration_info_from_tuplet_note (tuplet)
        else:
            return None

    def get_tuplet_note_count (self, tuplet_note):
        if tuplet_note:
            tuplet_nr = tuplet_note.get_maybe_exist_named_child ('tuplet-number')
            if tuplet_nr:
                return int (tuplet_nr.get_text ())
        return None
    def get_normal_nr (self):
        return self.get_tuplet_note_count (self.get_maybe_exist_named_child ('tuplet-normal'))
    def get_actual_nr (self):
        return self.get_tuplet_note_count (self.get_maybe_exist_named_child ('tuplet-actual'))

class Bracket (Music_xml_spanner):
    pass

class Dashes (Music_xml_spanner):
    pass

class Slur (Music_xml_spanner):
    def get_type (self):
        return self.type

class Beam (Music_xml_spanner):
    def get_type (self):
        return self.get_text ()
    def is_primary (self):
        if hasattr (self, 'number'):
            return self.number == "1"
        else:
            return True

class Wavy_line (Music_xml_spanner):
    pass

class Pedal (Music_xml_spanner):
    pass

class Glissando (Music_xml_spanner):
    pass

class Slide (Music_xml_spanner):
    pass

class Octave_shift (Music_xml_spanner):
    # default is 8 for the octave-shift!
    def get_size (self):
        if hasattr (self, 'size'):
            return string.atoi (self.size)
        else:
            return 8

class Chord (Music_xml_node):
    pass

class Dot (Music_xml_node):
    pass

# Rests in MusicXML are <note> blocks with a <rest> inside. This class is only
# for the inner <rest> element, not the whole rest block.
class Rest (Music_xml_node):
    def __init__ (self):
        Music_xml_node.__init__ (self)
        self._is_whole_measure = False
    def is_whole_measure (self):
        return self._is_whole_measure
    def get_step (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'display-step'))
        if ch:
            return ch.get_text ().strip ()
        else:
            return None
    def get_octave (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'display-octave'))
        if ch:
            oct = ch.get_text ().strip ()
            return int (oct)
        else:
            return None

class Type (Music_xml_node):
    pass
class Grace (Music_xml_node):
    pass
class Staff (Music_xml_node):
    pass

class Direction (Music_xml_node):
    pass
class DirType (Music_xml_node):
    pass

class Bend (Music_xml_node):
    def bend_alter (self):
        alter = self.get_maybe_exist_named_child ('bend-alter')
        return interpret_alter_element (alter)

class Words (Music_xml_node):
    pass

class Harmony (Music_xml_node):
    pass

class ChordPitch (Music_xml_node):
    def step_class_name (self):
        return u'root-step'
    def alter_class_name (self):
        return u'root-alter'
    def get_step (self):
        ch = self.get_unique_typed_child (get_class (self.step_class_name ()))
        return ch.get_text ().strip ()
    def get_alteration (self):
        ch = self.get_maybe_exist_typed_child (get_class (self.alter_class_name ()))
        return interpret_alter_element (ch)

class Root (ChordPitch):
    pass

class Bass (ChordPitch):
    def step_class_name (self):
        return u'bass-step'
    def alter_class_name (self):
        return u'bass-alter'

class ChordModification (Music_xml_node):
    def get_type (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'degree-type'))
        return {'add': 1, 'alter': 1, 'subtract': -1}.get (ch.get_text ().strip (), 0)
    def get_value (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'degree-value'))
        value = 0
        if ch:
            value = int (ch.get_text ().strip ())
        return value
    def get_alter (self):
        ch = self.get_maybe_exist_typed_child (get_class (u'degree-alter'))
        return interpret_alter_element (ch)


class Frame (Music_xml_node):
    def get_frets (self):
        return self.get_named_child_value_number ('frame-frets', 4)
    def get_strings (self):
        return self.get_named_child_value_number ('frame-strings', 6)
    def get_first_fret (self):
        return self.get_named_child_value_number ('first-fret', 1)

class Frame_Note (Music_xml_node):
    def get_string (self):
        return self.get_named_child_value_number ('string', 1)
    def get_fret (self):
        return self.get_named_child_value_number ('fret', 0)
    def get_fingering (self):
        return self.get_named_child_value_number ('fingering', -1)
    def get_barre (self):
        n = self.get_maybe_exist_named_child ('barre')
        if n:
            return getattr (n, 'type', '')
        else:
            return ''

class FiguredBass (Music_xml_node):
    pass

class Beats (Music_xml_node):
    pass

class BeatType (Music_xml_node):
    pass

class BeatUnit (Music_xml_node):
    pass

class BeatUnitDot (Music_xml_node):
    pass

class PerMinute (Music_xml_node):
    pass

class Print (Music_xml_node):
    pass



## need this, not all classes are instantiated
## for every input file. Only add those classes, that are either directly
## used by class name or extend Music_xml_node in some way!
class_dict = {
        '#comment': Hash_comment,
        '#text': Hash_text,
        'accidental': Accidental,
        'attributes': Attributes,
        'barline': Barline,
        'bar-style': BarStyle,
        'bass': Bass,
        'beam' : Beam,
        'beats': Beats,
        'beat-type': BeatType,
        'beat-unit': BeatUnit,
        'beat-unit-dot': BeatUnitDot,
        'bend' : Bend,
        'bracket' : Bracket,
        'chord': Chord,
        'dashes' : Dashes,
        'degree' : ChordModification,
        'dot': Dot,
        'direction': Direction,
        'direction-type': DirType,
        'duration': Duration,
        'elision': Elision,
        'extend': Extend,
        'frame': Frame,
        'frame-note': Frame_Note,
        'figured-bass': FiguredBass,
        'glissando': Glissando,
        'grace': Grace,
        'harmony': Harmony,
        'identification': Identification,
        'key-alter': KeyAlter,
        'key-octave': KeyOctave,
        'key-step': KeyStep,
        'lyric': Lyric,
        'measure': Measure,
        'notations': Notations,
        'note': Note,
        'octave-shift': Octave_shift,
        'part': Part,
    'part-group': Part_group,
        'part-list': Part_list,
        'pedal': Pedal,
        'per-minute': PerMinute,
        'pitch': Pitch,
        'print': Print,
        'rest': Rest,
        'root': Root,
        'score-part': Score_part,
        'slide': Slide,
        'slur': Slur,
        'staff': Staff,
        'syllabic': Syllabic,
        'text': Text,
        'time-modification': Time_modification,
        'tuplet': Tuplet,
        'type': Type,
        'unpitched': Unpitched,
        'wavy-line': Wavy_line,
        'wedge': Wedge,
        'words': Words,
        'work': Work,
}

def name2class_name (name):
    name = name.replace ('-', '_')
    name = name.replace ('#', 'hash_')
    name = name[0].upper() + name[1:].lower()

    return str (name)

def get_class (name):
    classname = class_dict.get (name)
    if classname:
        return classname
    else:
        class_name = name2class_name (name)
        klass = new.classobj (class_name, (Music_xml_node,) , {})
        class_dict[name] = klass
        return klass

def lxml_demarshal_node (node):
    name = node.tag

    # Ignore comment nodes, which are also returned by the etree parser!
    if name is None or node.__class__.__name__ == "_Comment":
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

    for (k, v) in node.items ():
        py_node.__dict__[k] = v
        py_node._attribute_dict[k] = v

    return py_node

def minidom_demarshal_node (node):
    name = node.nodeName

    klass = get_class (name)
    py_node = klass ()
    py_node._name = name
    py_node._children = [minidom_demarshal_node (cn) for cn in node.childNodes]
    for c in py_node._children:
        c._parent = py_node

    if node.attributes:
        for (nm, value) in node.attributes.items ():
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
    ks = class_dict.keys ()
    ks.sort ()
    print '\n'.join (ks)
