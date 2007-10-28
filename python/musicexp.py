import inspect
import sys
import string
import re

from rational import Rational


def escape_instrument_string (input_string):
    retstring = string.replace (input_string, "\"", "\\\"")
    if re.match ('.*[\r\n]+.*', retstring):
        rx = re.compile (r'[\n\r]+')
        strings = rx.split (retstring)
        retstring = "\\markup { \\column { "
        for s in strings:
            retstring += "\\line {\"" + s + "\"} "
        retstring += "} }"
    else:
        retstring = "\"" + retstring + "\""
    return retstring

class Output_stack_element:
    def __init__ (self):
        self.factor = Rational (1)
    def copy (self):
        o = Output_stack_element()
        o.factor = self.factor
        return o

class Output_printer:

    """A class that takes care of formatting (eg.: indenting) a
    Music expression as a .ly file.
    
    """
    ## TODO: support for \relative.
    
    def __init__ (self):
        self._line = ''
        self._indent = 4
        self._nesting = 0
        self._file = sys.stdout
        self._line_len = 72
        self._output_state_stack = [Output_stack_element()]
        self._skipspace = False
        self._last_duration = None

    def set_file (self, file):
        self._file = file
        
    def dump_version (self):
        self.newline ()
        self.print_verbatim ('\\version "@TOPLEVEL_VERSION@"')
        self.newline ()
        
    def get_indent (self):
        return self._nesting * self._indent
    
    def override (self):
        last = self._output_state_stack[-1]
        self._output_state_stack.append (last.copy())
        
    def add_factor (self, factor):
        self.override()
        self._output_state_stack[-1].factor *=  factor

    def revert (self):
        del self._output_state_stack[-1]
        if not self._output_state_stack:
            raise 'empty'

    def duration_factor (self):
        return self._output_state_stack[-1].factor

    def print_verbatim (self, str):
        self._line += str

    def unformatted_output (self, str):
        # don't indent on \< and indent only once on <<
        self._nesting += ( str.count ('<') 
                         - str.count ('\<') - str.count ('<<') 
                         + str.count ('{') )
        self._nesting -= ( str.count ('>') - str.count ('\>') - str.count ('>>')
                                           - str.count ('->') - str.count ('_>')
                                           - str.count ('^>')
                         + str.count ('}') )
        self.print_verbatim (str)
        
    def print_duration_string (self, str):
        if self._last_duration == str:
            return
        
        self.unformatted_output (str)
                  
    def add_word (self, str):
        if (len (str) + 1 + len (self._line) > self._line_len):
            self.newline()
            self._skipspace = True

        if not self._skipspace:
            self._line += ' '
        self.unformatted_output (str)
        self._skipspace = False
        
    def newline (self):
        self._file.write (self._line + '\n')
        self._line = ' ' * self._indent * self._nesting
        self._skipspace = True

    def skipspace (self):
        self._skipspace = True
        
    def __call__(self, arg):
        self.dump (arg)
    
    def dump (self, str):
        if self._skipspace:
            self._skipspace = False
            self.unformatted_output (str)
        else:
            words = string.split (str)
            for w in words:
                self.add_word (w)


    def close (self):
        self.newline ()
        self._file.close ()
        self._file = None
        
        
class Duration:
    def __init__ (self):
        self.duration_log = 0
        self.dots = 0
        self.factor = Rational (1)
        
    def lisp_expression (self):
        return '(ly:make-duration %d %d %d %d)' % (self.duration_log,
                             self.dots,
                             self.factor.numerator (),
                             self.factor.denominator ())


    def ly_expression (self, factor = None):
        if not factor:
            factor = self.factor
            
        str = '%d%s' % (1 << self.duration_log, '.'*self.dots)

        if factor <> Rational (1,1):
            if factor.denominator () <> 1:
                str += '*%d/%d' % (factor.numerator (), factor.denominator ())
            else:
                str += '*%d' % factor.numerator ()

        return str
    
    def print_ly (self, outputter):
        str = self.ly_expression (self.factor / outputter.duration_factor ())
        outputter.print_duration_string (str)
        
    def __repr__(self):
        return self.ly_expression()
        
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
        
    def __repr__(self):
        return self.ly_expression()

    def transposed (self, interval):
        c = self.copy ()
        c.alteration  += interval.alteration
        c.step += interval.step
        c.octave += interval.octave
        c.normalize ()
        
        target_st = self.semitones()  + interval.semitones()
        c.alteration += target_st - c.semitones()
        return c

    def normalize (c):
        while c.step < 0:
            c.step += 7
            c.octave -= 1
        c.octave += c.step / 7
        c.step = c.step  % 7

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
        return self.step + self.octave *7

    def semitones (self):
        return self.octave * 12 + [0,2,4,5,7,9,11][self.step] + self.alteration
    
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
    
    def print_ly (self, outputter):
        outputter (self.ly_expression())
    
class Music:
    def __init__ (self):
        self.parent = None
        self.start = Rational (0)
        self.comment = ''
        self.identifier = None
        
    def get_length(self):
        return Rational (0)
    
    def get_properties (self):
        return ''
    
    def has_children (self):
        return False
    
    def get_index (self):
        if self.parent:
            return self.parent.elements.index (self)
        else:
            return None
    def name (self):
        return self.__class__.__name__
    
    def lisp_expression (self):
        name = self.name()

        props = self.get_properties ()
        
        return "(make-music '%s %s)" % (name,  props)

    def set_start (self, start):
        self.start = start

    def find_first (self, predicate):
        if predicate (self):
            return self
        return None

    def print_comment (self, printer, text = None):
        if not text:
            text = self.comment

        if not text:
            return
            
        if text == '\n':
            printer.newline ()
            return
        
        lines = string.split (text, '\n')
        for l in lines:
            if l:
                printer.unformatted_output ('% ' + l)
            printer.newline ()
            

    def print_with_identifier (self, printer):
        if self.identifier: 
            printer ("\\%s" % self.identifier)
        else:
            self.print_ly (printer)

    def print_ly (self, printer):
        printer (self.ly_expression ())

class MusicWrapper (Music):
    def __init__ (self):
        Music.__init__(self)
        self.element = None
    def print_ly (self, func):
        self.element.print_ly (func)

class ModeChangingMusicWrapper (MusicWrapper):
    def __init__ (self):
        MusicWrapper.__init__ (self)
        self.mode = 'notemode'

    def print_ly (self, func):
        func ('\\%s' % self.mode)
        MusicWrapper.print_ly (self, func)

class TimeScaledMusic (MusicWrapper):
    def print_ly (self, func):
        func ('\\times %d/%d ' %
           (self.numerator, self.denominator))
        func.add_factor (Rational (self.numerator, self.denominator))
        MusicWrapper.print_ly (self, func)
        func.revert ()

class NestedMusic(Music):
    def __init__ (self):
        Music.__init__ (self)
        self.elements = []

    def append (self, what):
        if what:
            self.elements.append (what)
            
    def has_children (self):
        return self.elements

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
    def get_last_event_chord (self):
        value = None
        at = len( self.elements ) - 1
        while (at >= 0 and
               not isinstance (self.elements[at], EventChord) and
               not isinstance (self.elements[at], BarLine)):
            at -= 1

        if (at >= 0 and isinstance (self.elements[at], EventChord)):
            value = self.elements[at]
        return value

    def print_ly (self, printer, newline = True):
        printer ('{')
        if self.comment:
            self.print_comment (printer)

        if newline:
            printer.newline()
        for e in self.elements:
            e.print_ly (printer)

        printer ('}')
        if newline:
            printer.newline()
            
    def lisp_sub_expression (self, pred):
        name = self.name()


        props = self.get_subset_properties (pred)
        
        return "(make-music '%s %s)" % (name,  props)
    
    def set_start (self, start):
        for e in self.elements:
            e.set_start (start)
            start += e.get_length()

class RepeatedMusic:
    def __init__ (self):
        self.repeat_type = "volta"
        self.repeat_count = 2
        self.endings = []
        self.music = None
    def set_music (self, music):
        if isinstance (music, Music):
            self.music = music
        elif isinstance (music, list):
            self.music = SequentialMusic ()
            self.music.elements = music
        else:
            sys.stderr.write ("WARNING: Unable to set the music %s for the repeat %s" % (music, self))
    def add_ending (self, music):
        self.endings.append (music)
    def print_ly (self, printer):
        printer.dump ('\\repeat %s %s' % (self.repeat_type, self.repeat_count))
        if self.music:
            self.music.print_ly (printer)
        else:
            sys.stderr.write ("WARNING: Encountered repeat without body\n")
            printer.dump ('{}')
        if self.endings:
            printer.dump ('\\alternative {')
            for e in self.endings:
                e.print_ly (printer)
            printer.dump ('}')


class Lyrics:
    def __init__ (self):
        self.lyrics_syllables = []

    def print_ly (self, printer):
        printer.dump ("\lyricmode {")
        for l in self.lyrics_syllables:
            printer.dump ( "%s " % l )
        printer.dump ("}")

    def ly_expression (self):
        lstr = "\lyricmode {\n  "
        for l in self.lyrics_syllables:
            lstr += l + " "
        lstr += "\n}"
        return lstr


class Header:
    def __init__ (self):
        self.header_fields = {}
    def set_field (self, field, value):
        self.header_fields[field] = value

    def print_ly (self, printer):
        printer.dump ("\header {")
        printer.newline ()
        for (k,v) in self.header_fields.items ():
            if v:
                printer.dump ('%s = %s' % (k,v))
                printer.newline ()
        printer.dump ("}")
        printer.newline ()
        printer.newline ()




class EventChord (NestedMusic):
    def __init__ (self):
        NestedMusic.__init__ (self)
        self.grace_elements = None
        self.grace_type = None
    def append_grace (self, element):
        if element:
            if not self.grace_elements:
                self.grace_elements = SequentialMusic ()
            self.grace_elements.append (element)

    def get_length (self):
        l = Rational (0)
        for e in self.elements:
            l = max(l, e.get_length())
        return l
    
    def print_ly (self, printer):
        note_events = [e for e in self.elements if
               isinstance (e, NoteEvent)]

        rest_events = [e for e in self.elements if
               isinstance (e, RhythmicEvent)
               and not isinstance (e, NoteEvent)]
        
        other_events = [e for e in self.elements if
                not isinstance (e, RhythmicEvent)]

        if self.grace_elements and self.elements:
            if self.grace_type:
                printer ('\\%s' % self.grace_type)
            else:
                printer ('\\grace')
            # don't print newlines after the { and } braces
            self.grace_elements.print_ly (printer, False)

        if rest_events:
            rest_events[0].print_ly (printer)
        elif len (note_events) == 1:
            note_events[0].print_ly (printer)
        elif note_events:
            pitches = [x.pitch.ly_expression () for x in note_events]
            printer ('<%s>' % string.join (pitches))
            note_events[0].duration.print_ly (printer)
        else:
            pass
        
        for e in other_events:
            e.print_ly (printer)

        self.print_comment (printer)
            
class Partial (Music):
    def __init__ (self):
        Music.__init__ (self)
        self.partial = None
    def print_ly (self, printer):
        if self.partial:
            printer.dump ("\\partial %s" % self.partial.ly_expression ())

class BarLine (Music):
    def __init__ (self):
        Music.__init__ (self)
        self.bar_number = 0
        self.type = None
        
    def print_ly (self, printer):
        bar_symbol = { 'regular': "|", 'dotted': ":", 'dashed': ":",
                       'heavy': "|", 'light-light': "||", 'light-heavy': "|.",
                       'heavy-light': ".|", 'heavy-heavy': ".|.", 'tick': "'",
                       'short': "'", 'none': "" }.get (self.type, None)
        if bar_symbol <> None:
            printer.dump ('\\bar "%s"' % bar_symbol)
        else:
            printer.dump ("|")

        if self.bar_number > 0 and (self.bar_number % 10) == 0:
            printer.dump ("\\barNumberCheck #%d " % self.bar_number)
        else:
            printer.print_verbatim (' %% %d' % self.bar_number)
        printer.newline ()

    def ly_expression (self):
        return " | "

class Event(Music):
    pass

class SpanEvent (Event):
    def __init__ (self):
        Event.__init__ (self)
        self.span_direction = 0 # start/stop
        self.line_type = 'solid'
        self.span_type = 0 # e.g. cres/decrescendo, ottava up/down
        self.size = 0 # size of e.g. ocrave shift
    def wait_for_note (self):
        return True
    def get_properties(self):
        return "'span-direction  %d" % self.span_direction
    def set_span_type (self, type):
        self.span_type = type

class SlurEvent (SpanEvent):
    def ly_expression (self):
        before = ''
        after = ''
        # TODO: setting dashed/dotted line style does not work, because that
        #       command needs to be written before the note, not when the
        #       event is observed after the note!
        #before = {'dotted': '\\slurDotted', 
        #          'dashed' : '\\slurDashed'}.get (self.line_type, '')
        #if before:
            #after = '\\slurSolid'

        return {-1: before + '(' + after,
            1:')'}.get (self.span_direction, '')

class BeamEvent (SpanEvent):
    def ly_expression (self):
        return {-1: '[',
            1:']'}.get (self.span_direction, '')

class PedalEvent (SpanEvent):
    def ly_expression (self):
        return {-1: '\\sustainDown',
            1:'\\sustainUp'}.get (self.span_direction, '')

# type==-1 means octave up, type==-2 means octave down
class OctaveShiftEvent (SpanEvent):
    def wait_for_note (self):
        return False;
    def set_span_type (self, type):
        self.span_type = {'up': 1, 'down': -1}.get (type, 0)
    def ly_octave_shift_indicator (self):
        # convert 8/15 to lilypond indicators (+-1/+-2)
        value = {8: 1, 15: 2}.get (self.size, 0)
        # negative values go up!
        value *= -1*self.span_type
        return value
    def ly_expression (self):
        dir = self.ly_octave_shift_indicator ()
        value = ''
        if dir:
            value = '#(set-octavation %s)' % dir
        return { 
            -1: value,
            1: '#(set-octavation 0)'}.get (self.span_direction, '')

class TrillSpanEvent (SpanEvent):
    def ly_expression (self):
        return {-1: '\\startTrillSpan',
            0: '', # no need to write out anything for type='continue'
            1:'\\stopTrillSpan'}.get (self.span_direction, '')

class GlissandoEvent (SpanEvent):
    def ly_expression (self):
        style = ''
        # TODO: wavy-line glissandos don't work, becasue the style has to be
        #       set before the note, at the \glissando it's already too late!
        #if self.line_type == 'wavy':
            #style = "\once\override Glissando #'style = #'zigzag"
        # In lilypond, glissando is NOT a spanner, unlike MusicXML.
        return {-1: style + '\\glissando',
            1:''}.get (self.span_direction, '')

class ArpeggioEvent(Event):
    def wait_for_note (self):
        return True;
    def ly_expression (self):
        return ('\\arpeggio')


class TieEvent(Event):
    def ly_expression (self):
        return '~'


class HairpinEvent (SpanEvent):
    def set_span_type (self, type):
        self.span_type = {'crescendo' : 1, 'decrescendo' : -1, 'diminuendo' : -1 }.get (type, 0)
    def hairpin_to_ly (self):
        if self.span_direction == 1:
            return '\!'
        else:
            return {1: '\<', -1: '\>'}.get (self.span_type, '')
    
    def ly_expression (self):
        return self.hairpin_to_ly ()
    
    def print_ly (self, printer):
        val = self.hairpin_to_ly ()
        if val:
            printer.dump (val)



class DynamicsEvent (Event):
    def __init__ (self):
        self.type = None
        self.available_commands = [ "ppppp", "pppp", "ppp", "pp", "p", 
                                    "mp", "mf", 
                                    "f", "ff", "fff", "ffff", 
                                    "fp", "sf", "sff", "sp", "spp", "sfz", "rfz" ];
    def wait_for_note (self):
        return True;
    def ly_expression (self):
        if self.type == None:
            return;
        elif self.type in self.available_commands:
            return '\%s' % self.type
        else:
            return '-\markup{ \dynamic %s }' % self.type
        
    def print_ly (self, printer):
        if self.type == None:
            return
        elif self.type in self.available_commands:
            printer.dump ("\\%s" % self.type)
        else:
            printer.dump ("-\\markup{ \\dynamic %s }" % self.type)


class TextEvent (Event):
    def __init__ (self):
        self.Text = None
        self.force_direction = None
        self.markup = ''
    def wait_for_note (self):
        return True

    def direction_mod (self):
        return { 1: '^', -1: '_', 0: '-' }.get (self.force_direction, '-')

    def ly_expression (self):
        base_string = '%s\"%s\"'
        if self.markup:
            base_string = '%s\markup{ ' + self.markup + ' {%s} }'
        return base_string % (self.direction_mod (), self.text)

class ArticulationEvent (Event):
    def __init__ (self):
        self.type = None
        self.force_direction = None
    def wait_for_note (self):
        return True;

    def direction_mod (self):
        return { 1: '^', -1: '_', 0: '-' }.get (self.force_direction, '')

    def ly_expression (self):
        return '%s\\%s' % (self.direction_mod (), self.type)

class ShortArticulationEvent (ArticulationEvent):
    def direction_mod (self):
        # default is -
        return { 1: '^', -1: '_', 0: '-' }.get (self.force_direction, '-')
    def ly_expression (self):
        return '%s%s' % (self.direction_mod (), self.type)

class NoDirectionArticulationEvent (ArticulationEvent):
    def ly_expression (self):
        return '\\%s' % self.type

class MarkupEvent (ShortArticulationEvent):
    def __init__ (self):
        ArticulationEvent.__init__ (self)
        self.contents = None
    def ly_expression (self):
        if self.contents:
            return "%s\\markup { %s }" % (self.direction_mod (), self.contents)
        else:
            return ''

class FretEvent (MarkupEvent):
    def __init__ (self):
        MarkupEvent.__init__ (self)
        self.force_direction = 1
        self.strings = 6
        self.frets = 4
        self.barre = None
        self.elements = []
    def ly_expression (self):
        val = ""
        if self.strings <> 6:
            val += "w:%s;" % self.strings
        if self.frets <> 4:
            val += "h:%s;" % self.frets
        if self.barre and len (self.barre) >= 3:
            val += "c:%s-%s-%s;" % (self.barre[0], self.barre[1], self.barre[2])
        have_fingering = False
        for i in self.elements:
            if len (i) > 1:
                val += "%s-%s" % (i[0], i[1])
            if len (i) > 2:
                have_fingering = True
                val += "-%s" % i[2]
            val += ";"
        if have_fingering:
            val = "f:1;" + val
        if val:
            return "%s\\markup { \\fret-diagram #\"%s\" }" % (self.direction_mod (), val)
        else:
            return ''

class TremoloEvent (ArticulationEvent):
    def __init__ (self):
        Event.__init__ (self)
        self.bars = 0

    def ly_expression (self):
        str=''
        if self.bars and self.bars > 0:
            str += ':%s' % (2 ** (2 + string.atoi (self.bars)))
        return str

class BendEvent (ArticulationEvent):
    def __init__ (self):
        Event.__init__ (self)
        self.alter = 0
    def ly_expression (self):
        if self.alter:
            return "-\\bendAfter #%s" % self.alter
        else:
            return ''

class RhythmicEvent(Event):
    def __init__ (self):
        Event.__init__ (self)
        self.duration = Duration()
        
    def get_length (self):
        return self.duration.get_length()
        
    def get_properties (self):
        return ("'duration %s"
                % self.duration.lisp_expression ())
    
class RestEvent (RhythmicEvent):
    def __init__ (self):
        RhythmicEvent.__init__ (self)
        self.pitch = None
    def ly_expression (self):
        if self.pitch:
            return "%s%s\\rest" % (self.pitch.ly_expression (), self.duration.ly_expression ())
        else:
            return 'r%s' % self.duration.ly_expression ()
    
    def print_ly (self, printer):
        if self.pitch:
            self.pitch.print_ly (printer)
            self.duration.print_ly (printer)
            printer ('\\rest')
        else:
            printer('r')
            self.duration.print_ly (printer)

class SkipEvent (RhythmicEvent):
    def ly_expression (self):
        return 's%s' % self.duration.ly_expression () 

class NoteEvent(RhythmicEvent):
    def  __init__ (self):
        RhythmicEvent.__init__ (self)
        self.pitch = None
        self.drum_type = None
        self.cautionary = False
        self.forced_accidental = False
        
    def get_properties (self):
        str = RhythmicEvent.get_properties (self)
        
        if self.pitch:
            str += self.pitch.lisp_expression ()
        elif self.drum_type:
            str += "'drum-type '%s" % self.drum_type

        return str
    
    def pitch_mods (self):
        excl_question = ''
        if self.cautionary:
            excl_question += '?'
        if self.forced_accidental:
            excl_question += '!'

        return excl_question
    
    def ly_expression (self):
        if self.pitch:
            return '%s%s%s' % (self.pitch.ly_expression (),
                               self.pitch_mods(),
                               self.duration.ly_expression ())
        elif self.drum_type:
            return '%s%s' (self.drum_type,
                           self.duration.ly_expression ())

    def print_ly (self, printer):
        if self.pitch:
            self.pitch.print_ly (printer)
            printer (self.pitch_mods ())
        else:
            printer (self.drum_type)

        self.duration.print_ly (printer)

class KeySignatureChange (Music):
    def __init__ (self):
        Music.__init__ (self)
        self.scale = []
        self.tonic = Pitch()
        self.mode = 'major'
        
    def ly_expression (self):
        return '\\key %s \\%s' % (self.tonic.ly_step_expression (),
                     self.mode)
    
    def lisp_expression (self):
        pairs = ['(%d . %d)' % (i , self.scale[i]) for i in range (0,7)]
        scale_str = ("'(%s)" % string.join (pairs))

        return """ (make-music 'KeyChangeEvent
     'pitch-alist %s) """ % scale_str

class TimeSignatureChange (Music):
    def __init__ (self):
        Music.__init__ (self)
        self.fraction = (4,4)
    def ly_expression (self):
        return '\\time %d/%d ' % self.fraction
    
class ClefChange (Music):
    def __init__ (self):
        Music.__init__ (self)
        self.type = 'G'
        self.position = 2
        self.octave = 0

    def octave_modifier (self):
        return {1: "^8", 2: "^15", -1: "_8", -2: "_15"}.get (self.octave, '')
    def clef_name (self):
        return {('G', 2): "treble",
                ('G', 1): "french",
                ('C', 1): "soprano",
                ('C', 2): "mezzosoprano",
                ('C', 3): "alto",
                ('C', 4): "tenor",
                ('C', 5): "baritone",
                ('F', 3): "varbaritone",
                ('F', 4): "bass",
                ('F', 5): "subbass",
                ("percussion", 2): "percussion",
                ("TAB", 5): "tab"}.get ((self.type, self.position), None)
    def ly_expression (self):
        return '\\clef "%s%s"' % (self.clef_name (), self.octave_modifier ())

    clef_dict = {
        "G": ("clefs.G", -2, -6),
        "C": ("clefs.C", 0, 0),
        "F": ("clefs.F", 2, 6),
        }
    
    def lisp_expression (self):
        try:
            (glyph, pos, c0) = self.clef_dict[self.type]
        except KeyError:
            return ""
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


class StaffChange (Music):
    def __init__ (self, staff):
        Music.__init__ (self)
        self.staff = staff
    def ly_expression (self):
        if self.staff:
            return "\\change Staff=\"%s\"" % self.staff
        else:
            return ''


class MultiMeasureRest(Music):

    def lisp_expression (self):
        return """
(make-music
  'MultiMeasureRestMusicGroup
  'elements
  (list (make-music (quote BarCheck))
        (make-music
          'EventChord
          'elements
          (list (make-music
                  'MultiMeasureRestEvent
                  'duration
                  %s)))
        (make-music (quote BarCheck))))
""" % self.duration.lisp_expression ()

    def ly_expression (self):
        return 'R%s' % self.duration.ly_expression ()


class StaffGroup:
    def __init__ (self, command = "StaffGroup"):
        self.stafftype = command
        self.id = None
        self.instrument_name = None
        self.short_instrument_name = None
        self.symbol = None
        self.spanbar = None
        self.children = []
        self.is_group = True
        # part_information is a list with entries of the form
        #     [staffid, voicelist]
        # where voicelist is a list with entries of the form
        #     [voiceid1, [lyricsid11, lyricsid12,...] ]
        self.part_information = None

    def append_staff (self, staff):
        self.children.append (staff)

    def set_part_information (self, part_name, staves_info):
        if part_name == self.id:
            self.part_information = staves_info
        else:
            for c in self.children:
                c.set_part_information (part_name, staves_info)

    def print_ly_contents (self, printer):
        for c in self.children:
            if c:
                c.print_ly (printer)
    def print_ly_overrides (self, printer):
        needs_with = False
        needs_with |= self.spanbar == "no"
        needs_with |= self.instrument_name != None
        needs_with |= self.short_instrument_name != None
        needs_with |= (self.symbol != None) and (self.symbol != "bracket")
        if needs_with:
            printer.dump ("\\with {")
            if self.instrument_name or self.short_instrument_name:
                printer.dump ("\\consists \"Instrument_name_engraver\"")
            if self.spanbar == "no":
                printer.dump ("\\override SpanBar #'transparent = ##t")
            brack = {"brace": "SystemStartBrace",
                     "none": "f",
                     "line": "SystemStartSquare"}.get (self.symbol, None)
            if brack:
                printer.dump ("systemStartDelimiter = #'%s" % brack)
            printer.dump ("}")

    def print_ly (self, printer):
        if self.stafftype:
            printer.dump ("\\new %s" % self.stafftype)
        self.print_ly_overrides (printer)
        printer.dump ("<<")
        printer.newline ()
        if self.stafftype and self.instrument_name:
            printer.dump ("\\set %s.instrumentName = %s" % (self.stafftype, 
                    escape_instrument_string (self.instrument_name)))
            printer.newline ()
        if self.stafftype and self.short_instrument_name:
            printer.dump ("\\set %s.shortInstrumentName = %s" % (self.stafftype,
                    escape_instrument_string (self.short_instrument_name)))
            printer.newline ()
        self.print_ly_contents (printer)
        printer.newline ()
        printer.dump (">>")
        printer.newline ()


class Staff (StaffGroup):
    def __init__ (self):
        StaffGroup.__init__ (self, "Staff")
        self.is_group = False
        self.part = None

    def print_ly_overrides (self, printer):
        pass

    def print_ly_contents (self, printer):
        if not self.id or not self.part_information:
            return

        for [staff_id, voices] in self.part_information:
            if staff_id:
                printer ('\\context Staff = "%s" << ' % staff_id)
            else:
                printer ('\\context Staff << ')
            printer.newline ()
            n = 0
            nr_voices = len (voices)
            for [v, lyrics] in voices:
                n += 1
                voice_count_text = ''
                if nr_voices > 1:
                    voice_count_text = {1: ' \\voiceOne', 2: ' \\voiceTwo',
                                        3: ' \\voiceThree'}.get (n, ' \\voiceFour')
                printer ('\\context Voice = "%s" {%s \\%s }' % (v,voice_count_text,v))
                printer.newline ()

                for l in lyrics:
                    printer ('\\new Lyrics \\lyricsto "%s" \\%s' % (v,l))
                    printer.newline()
            printer ('>>')

    def print_ly (self, printer):
        if self.part_information and len (self.part_information) > 1:
            self.stafftype = "PianoStaff"
        StaffGroup.print_ly (self, printer)


def test_pitch ():
    bflat = Pitch()
    bflat.alteration = -1
    bflat.step =  6
    bflat.octave = -1
    fifth = Pitch()
    fifth.step = 4
    down = Pitch ()
    down.step = -4
    down.normalize ()
    
    
    print bflat.semitones()
    print bflat.transposed (fifth),  bflat.transposed (fifth).transposed (fifth)
    print bflat.transposed (fifth).transposed (fifth).transposed (fifth)

    print bflat.semitones(), 'down'
    print bflat.transposed (down)
    print bflat.transposed (down).transposed (down)
    print bflat.transposed (down).transposed (down).transposed (down)



def test_printer ():
    def make_note ():
        evc = EventChord()
        n = NoteEvent()
        evc.append (n)
        return n

    def make_tup ():
        m = SequentialMusic()
        m.append (make_note ())
        m.append (make_note ())
        m.append (make_note ())

        
        t = TimeScaledMusic ()
        t.numerator = 2
        t.denominator = 3
        t.element = m
        return t

    m = SequentialMusic ()
    m.append (make_tup ())
    m.append (make_tup ())
    m.append (make_tup ())
    
    printer = Output_printer()
    m.print_ly (printer)
    printer.newline ()
    
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

    evc = ClefChange()
    evc.type = 'treble'
    m.insert_around (None, evc, 0)

    evc = EventChord()
    tonic = Pitch ()
    tonic.step = 2
    tonic.alteration = -2
    n = KeySignatureChange()
    n.tonic=tonic.copy()
    n.scale = [0, 0, -2, 0, 0,-2,-2]
    
    evc.insert_around (None, n, 0)
    m.insert_around (None, evc, 0)

    return m


if __name__ == '__main__':
    test_printer ()
    raise 'bla'
    test_pitch()
    
    expr = test_expr()
    expr.set_start (Rational (0))
    print expr.ly_expression()
    start = Rational (0,4)
    stop = Rational (4,2)
    def sub(x, start=start, stop=stop):
        ok = x.start >= start and x.start +x.get_length() <= stop
        return ok
    
    print expr.lisp_sub_expression(sub)

