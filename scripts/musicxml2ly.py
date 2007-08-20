#!@TARGET_PYTHON@

import optparse
import sys
import re
import os
import string
from gettext import gettext as _

"""
@relocate-preamble@
"""

import lilylib as ly

import musicxml
import musicexp

from rational import Rational


def progress (str):
    sys.stderr.write (str + '\n')
    sys.stderr.flush ()
    

def musicxml_duration_to_lily (mxl_note):
    d = musicexp.Duration ()
    if mxl_note.get_maybe_exist_typed_child (musicxml.Type):
        d.duration_log = mxl_note.get_duration_log ()
    else:
        d.duration_log = 0

    d.dots = len (mxl_note.get_typed_children (musicxml.Dot))
    d.factor = mxl_note._duration / d.get_length ()

    return d         

def group_tuplets (music_list, events):


    """Collect Musics from
    MUSIC_LIST demarcated by EVENTS_LIST in TimeScaledMusic objects.
    """

    
    indices = []

    j = 0
    for (ev_chord, tuplet_elt, fraction) in events:
        while (j < len (music_list)):
            if music_list[j] == ev_chord:
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


def musicxml_clef_to_lily (attributes):
    change = musicexp.ClefChange ()
    change.type = attributes.get_clef_sign ()
    return change
    
def musicxml_time_to_lily (attributes):
    (beats, type) = attributes.get_time_signature ()

    change = musicexp.TimeSignatureChange()
    change.fraction = (beats, type)
    
    return change

def musicxml_key_to_lily (attributes):
    start_pitch  = musicexp.Pitch ()
    (fifths, mode) = attributes.get_key_signature () 
    try:
        (n,a) = {
            'major' : (0,0),
            'minor' : (5,0),
            }[mode]
        start_pitch.step = n
        start_pitch.alteration = a
    except  KeyError:
        print 'unknown mode', mode

    fifth = musicexp.Pitch()
    fifth.step = 4
    if fifths < 0:
        fifths *= -1
        fifth.step *= -1
        fifth.normalize ()
    
    for x in range (fifths):
        start_pitch = start_pitch.transposed (fifth)

    start_pitch.octave = 0

    change = musicexp.KeySignatureChange()
    change.mode = mode
    change.tonic = start_pitch
    return change
    
def musicxml_attributes_to_lily (attrs):
    elts = []
    attr_dispatch =  {
        'clef': musicxml_clef_to_lily,
        'time': musicxml_time_to_lily,
        'key': musicxml_key_to_lily
    }
    for (k, func) in attr_dispatch.items ():
        children = attrs.get_named_children (k)

        ## ugh: you get clefs spread over staves for piano
        if children:
            elts.append (func (attrs))
    
    return elts

spanner_event_dict = {
    'slur' : musicexp.SlurEvent,
    'beam' : musicexp.BeamEvent,
}        
spanner_type_dict = {
    'start': -1,
    'begin': -1,
    'stop': 1,
    'end' : 1
}

def musicxml_spanner_to_lily_event (mxl_event):
    ev = None
    
    name = mxl_event.get_name()
    func = spanner_event_dict.get (name)
    if func:
        ev = func()
    else:
        print 'unknown span event ', mxl_event

    key = mxl_event.get_type ()
    span_direction = spanner_type_dict.get (key)
    if span_direction:
        ev.span_direction = span_direction
    else:
        print 'unknown span type', key, 'for', name

    return ev

def musicxml_direction_to_indicator (direction):
    return { "above": 1, "upright": 1, "below": -1, "downright": -1 }.get (direction, '')

def musicxml_fermata_to_lily_event (mxl_event):
    ev = musicexp.ArticulationEvent ()
    ev.type = "fermata"
    if hasattr (mxl_event, 'type'):
      dir = musicxml_direction_to_indicator (mxl_event.type)
      if dir:
        ev.force_direction = dir
    return ev

def musicxml_tremolo_to_lily_event(mxl_event):
    if mxl_event.get_name () != "tremolo": 
        return
    ev = musicexp.TremoloEvent ()
    ev.bars = mxl_event.get_text ()
    return ev

# TODO: Some translations are missing!
articulations_dict = { 
    ##### ORNAMENTS
    "trill-mark": "trill", 
    "turn": "turn", 
    #"delayed-turn": "?", 
    "inverted-turn": "reverseturn", 
    #"shake": "?", 
    #"wavy-line": "?", 
    "mordent": "mordent",
    #"inverted-mordent": "?", 
    #"schleifer": "?" 
    ##### TECHNICALS
    "up-bow": "upbow", 
    "down-bow": "downbow", 
    #"harmonic": "", 
    #"open-string": "", 
    #"thumb-position": "", 
    #"fingering": "", 
    #"pluck": "", 
    #"double-tongue": "", 
    #"triple-tongue": "", 
    #"stopped": "", 
    #"snap-pizzicato": "", 
    #"fret": "", 
    #"string": "", 
    #"hammer-on": "", 
    #"pull-off": "", 
    #"bend": "", 
    #"tap": "", 
    #"heel": "", 
    #"toe": "", 
    #"fingernails": ""
    ##### ARTICULATIONS
    "accent": "accent", 
    "strong-accent": "marcato", 
    "staccato": "staccato", 
    "tenuto": "tenuto", 
    #"detached-legato": "", 
    "staccatissimo": "staccatissimo", 
    #"spiccato": "", 
    #"scoop": "", 
    #"plop": "", 
    #"doit": "", 
    #"falloff": "",
    "breath-mark": "breathe", 
    #"caesura": "caesura", 
    #"stress": "", 
    #"unstress": ""
}

def musicxml_articulation_to_lily_event(mxl_event):
    ev = musicexp.ArticulationEvent ()
    tp = articulations_dict.get (mxl_event.get_name ())
    if not tp:
        return
    
    ev.type = tp

    # Some articulations use the type attribute, other the placement...
    dir = None
    if hasattr (mxl_event, 'type'):
        dir = musicxml_direction_to_indicator (mxl_event.type)
    if hasattr (mxl_event, 'placement'):
        dir = musicxml_direction_to_indicator (mxl_event.placement)
    if dir:
        ev.force_direction = dir
    return ev


def musicxml_direction_to_lily( n ):
    # TODO: Handle the <staff> element!
    res = []
    dirtype = n.get_maybe_exist_typed_child (musicxml.DirType)
    if not dirtype: 
      return res

    for entry in dirtype.get_all_children ():
        if entry.get_name () == "dynamics":
            for dynentry in entry.get_all_children ():
                dynamics_available = ( "p", "pp", "ppp", "pppp", "ppppp", "pppppp", 
                    "f", "ff", "fff", "ffff", "fffff", "ffffff", 
                    "mp", "mf", "sf", "sfp", "sfpp", "fp", 
                    "rf", "rfz", "sfz", "sffz", "fz" )
                if not dynentry.get_name() in dynamics_available: 
                    continue
                event = musicexp.DynamicsEvent ()
                event.type = dynentry.get_name ()
                res.append (event)
      
        if entry.get_name() == "wedge":
            if hasattr (entry, 'type'):
                wedgetype = entry.type;
                wedgetypeval = {"crescendo" : 1, "decrescendo" : -1, 
                                "diminuendo" : -1, "stop" : 0 }.get (wedgetype)
                # Really check for != None, becaus otherwise 0 will also cause 
                # the code to be executed!
                if wedgetypeval != None:
                    event = musicexp.HairpinEvent (wedgetypeval)
                    res.append (event)

    return res

instrument_drumtype_dict = {
    'Acoustic Snare Drum': 'acousticsnare',
    'Side Stick': 'sidestick',
    'Open Triangle': 'opentriangle',
    'Mute Triangle': 'mutetriangle',
    'Tambourine': 'tambourine'
}

def musicxml_note_to_lily_main_event (n):
    pitch  = None
    duration = None
        
    mxl_pitch = n.get_maybe_exist_typed_child (musicxml.Pitch)
    event = None
    if mxl_pitch:
        pitch = musicxml_pitch_to_lily (mxl_pitch)
        event = musicexp.NoteEvent()
        event.pitch = pitch

        acc = n.get_maybe_exist_named_child ('accidental')
        if acc:
            # let's not force accs everywhere. 
            event.cautionary = acc.editorial
        
    elif n.get_maybe_exist_typed_child (musicxml.Rest):
        event = musicexp.RestEvent()
    elif n.instrument_name:
        event = musicexp.NoteEvent ()
        drum_type = instrument_drumtype_dict.get (n.instrument_name)
        if drum_type:
            event.drum_type = drum_type
        else:
            n.message ("drum %s type unknow, please add to instrument_drumtype_dict" % n.instrument_name)
            event.drum_type = 'acousticsnare'
    
    if not event:
        n.message ("cannot find suitable event")

    event.duration = musicxml_duration_to_lily (n)
    return event


## todo
class NegativeSkip:
    def __init__ (self, here, dest):
        self.here = here
        self.dest = dest

class LilyPondVoiceBuilder:
    def __init__ (self):
        self.elements = []
        self.pending_dynamics = []
        self.end_moment = Rational (0)
        self.begin_moment = Rational (0)
        self.pending_multibar = Rational (0)

    def _insert_multibar (self):
        r = musicexp.MultiMeasureRest ()
        r.duration = musicexp.Duration()
        r.duration.duration_log = 0
        r.duration.factor = self.pending_multibar
        self.elements.append (r)
        self.begin_moment = self.end_moment
        self.end_moment = self.begin_moment + self.pending_multibar
        self.pending_multibar = Rational (0)
        
    def add_multibar_rest (self, duration):
        self.pending_multibar += duration
        
        
    def add_music (self, music, duration):
        assert isinstance (music, musicexp.Music)
        if self.pending_multibar > Rational (0):
            self._insert_multibar ()

        self.elements.append (music)
        self.begin_moment = self.end_moment
        self.end_moment = self.begin_moment + duration 

        # Insert all pending dynamics right after the note/rest:
        if duration > Rational (0):
            for d in self.pending_dynamics:
                self.elements.append (d)
            self.pending_dynamics = []

    def add_dynamics (self, dynamic):
        # store the dynamic item(s) until we encounter the next note/rest:
        self.pending_dynamics.append (dynamic)

    def add_bar_check (self, number):
        b = musicexp.BarCheck ()
        b.bar_number = number
        self.add_music (b, Rational (0))

    def jumpto (self, moment):
        current_end = self.end_moment + self.pending_multibar
        diff = moment - current_end
        
        if diff < Rational (0):
            print 'Negative skip', diff
            diff = Rational (0)

        if diff > Rational (0):
            skip = musicexp.SkipEvent()
            skip.duration.duration_log = 0
            skip.duration.factor = diff

            evc = musicexp.EventChord ()
            evc.elements.append (skip)
            self.add_music (evc, diff)
                
    def last_event_chord (self, starting_at):

        value = None
        if (self.elements
            and isinstance (self.elements[-1], musicexp.EventChord)
            and self.begin_moment == starting_at):
            value = self.elements[-1]
        else:
            self.jumpto (starting_at)
            value = None

        return value
        
    def correct_negative_skip (self, goto):
        self.end_moment = goto
        self.begin_moment = goto
        evc = musicexp.EventChord ()
        self.elements.append (evc)
        
def musicxml_voice_to_lily_voice (voice):
    tuplet_events = []
    modes_found = {}

    voice_builder = LilyPondVoiceBuilder()

    for n in voice._elements:
        if n.get_name () == 'forward':
            continue

        if isinstance (n, musicxml.Direction):
            for a in musicxml_direction_to_lily (n):
                voice_builder.add_dynamics (a)
            continue
        
        if not n.get_maybe_exist_named_child ('chord'):
            try:
                voice_builder.jumpto (n._when)
            except NegativeSkip, neg:
                voice_builder.correct_negative_skip (n._when)
                n.message ("Negative skip? from %s to %s, diff %s" % (neg.here, neg.dest, neg.dest - neg.here))
            
        if isinstance (n, musicxml.Attributes):
            if n.is_first () and n._measure_position == Rational (0):
                try:
                    number = int (n.get_parent ().number)
                except ValueError:
                    number = 0
                
                voice_builder.add_bar_check (number)
            for a in musicxml_attributes_to_lily (n):
                voice_builder.add_music (a, Rational (0))
            continue

        if not n.__class__.__name__ == 'Note':
            print 'not a Note or Attributes?', n
            continue

        rest = n.get_maybe_exist_typed_child (musicxml.Rest)
        if (rest
            and rest.is_whole_measure ()):

            voice_builder.add_multibar_rest (n._duration)
            continue

        if n.is_first () and n._measure_position == Rational (0):
            try: 
                num = int (n.get_parent ().number)
            except ValueError:
                num = 0
            voice_builder.add_bar_check (num)
        
        main_event = musicxml_note_to_lily_main_event (n)

        if hasattr (main_event, 'drum_type') and main_event.drum_type:
            modes_found['drummode'] = True


        ev_chord = voice_builder.last_event_chord (n._when)
        if not ev_chord: 
            ev_chord = musicexp.EventChord()
            voice_builder.add_music (ev_chord, n._duration)

        ev_chord.append (main_event)
        
        notations = n.get_maybe_exist_typed_child (musicxml.Notations)
        tuplet_event = None
        span_events = []
        
        # The <notation> element can have the following children (+ means implemented, ~ partially, - not):
        # +tied | +slur | +tuplet | glissando | slide | 
        #    ornaments | technical | articulations | dynamics |
        #    +fermata | arpeggiate | non-arpeggiate | 
        #    accidental-mark | other-notation
        if notations:
            if notations.get_tuplet():
                tuplet_event = notations.get_tuplet()
                mod = n.get_maybe_exist_typed_child (musicxml.Time_modification)
                frac = (1,1)
                if mod:
                    frac = mod.get_fraction ()
                
                tuplet_events.append ((ev_chord, tuplet_event, frac))

            slurs = [s for s in notations.get_named_children ('slur')
                if s.get_type () in ('start','stop')]
            if slurs:
                if len (slurs) > 1:
                    print 'more than 1 slur?'

                lily_ev = musicxml_spanner_to_lily_event (slurs[0])
                ev_chord.append (lily_ev)

            mxl_tie = notations.get_tie ()
            if mxl_tie and mxl_tie.type == 'start':
                ev_chord.append (musicexp.TieEvent ())
                
            fermatas = notations.get_named_children ('fermata')
            for a in fermatas:
                ev = musicxml_fermata_to_lily_event (a);
                if ev: 
                    ev_chord.append (ev)
                
            # Articulations can contain the following child elements:
            #         accent | strong-accent | staccato | tenuto |
            #         detached-legato | staccatissimo | spiccato |
            #         scoop | plop | doit | falloff | breath-mark | 
            #         caesura | stress | unstress
            # Technical can contain the following child elements:
            #         up-bow | down-bow | harmonic | open-string |
            #         thumb-position | fingering | pluck | double-tongue |
            #         triple-tongue | stopped | snap-pizzicato | fret |
            #         string | hammer-on | pull-off | bend | tap | heel |
            #         toe | fingernails | other-technical
            # Ornaments can contain the following child elements:
            #         trill-mark | turn | delayed-turn | inverted-turn |
            #         shake | wavy-line | mordent | inverted-mordent | 
            #         schleifer | tremolo | other-ornament, accidental-mark
            ornaments = notations.get_named_children ('ornaments')
            for a in ornaments:
                for ch in a.get_named_children ('tremolo'):
                    ev = musicxml_tremolo_to_lily_event (ch)
                    if ev: 
                        ev_chord.append (ev)

            ornaments += notations.get_named_children ('articulations')
            ornaments += notations.get_named_children ('technical')

            for a in ornaments:
                for ch in a.get_all_children ():
                    ev = musicxml_articulation_to_lily_event (ch)
                    if ev: 
                        ev_chord.append (ev)

            dynamics = notations.get_named_children ('dynamics')
            for a in dynamics:
                for ch in a.get_all_children ():
                    ev = musicxml_dynamics_to_lily_event (ch)
                    if ev:
                        ev_chord.append (ev)

        mxl_beams = [b for b in n.get_named_children ('beam')
                     if (b.get_type () in ('begin', 'end')
                         and b.is_primary ())] 
        if mxl_beams:
            beam_ev = musicxml_spanner_to_lily_event (mxl_beams[0])
            if beam_ev:
                ev_chord.append (beam_ev)
            
        if tuplet_event:
            mod = n.get_maybe_exist_typed_child (musicxml.Time_modification)
            frac = (1,1)
            if mod:
                frac = mod.get_fraction ()
                
            tuplet_events.append ((ev_chord, tuplet_event, frac))

    ## force trailing mm rests to be written out.   
    voice_builder.add_music (musicexp.EventChord (), Rational (0))
    
    ly_voice = group_tuplets (voice_builder.elements, tuplet_events)

    seq_music = musicexp.SequentialMusic()

    if 'drummode' in modes_found.keys ():
        ## \key <pitch> barfs in drummode.
        ly_voice = [e for e in ly_voice
                    if not isinstance(e, musicexp.KeySignatureChange)]
    
    seq_music.elements = ly_voice

    
    
    if len (modes_found) > 1:
       print 'Too many modes found', modes_found.keys ()

    return_value = seq_music
    for mode in modes_found.keys ():
        v = musicexp.ModeChangingMusicWrapper()
        v.element = return_value
        v.mode = mode
        return_value = v
    
    return return_value


def musicxml_id_to_lily (id):
    digits = ['Zero', 'One', 'Two', 'Three', 'Four', 'Five',
              'Six', 'Seven', 'Eight', 'Nine', 'Ten']
    
    for digit in digits:
        d = digits.index (digit)
        id = re.sub ('%d' % d, digit, id)

    id = re.sub  ('[^a-zA-Z]', 'X', id)
    return id


def musicxml_pitch_to_lily (mxl_pitch):
    p = musicexp.Pitch()
    p.alteration = mxl_pitch.get_alteration ()
    p.step = (ord (mxl_pitch.get_step ()) - ord ('A') + 7 - 2) % 7
    p.octave = mxl_pitch.get_octave () - 4
    return p

def voices_in_part (part):
    """Return a Name -> Voice dictionary for PART"""
    part.interpret ()
    part.extract_voices ()
    voice_dict = part.get_voices ()

    return voice_dict

def voices_in_part_in_parts (parts):
    """return a Part -> Name -> Voice dictionary"""
    return dict([(p, voices_in_part (p)) for p in parts])


def get_all_voices (parts):
    all_voices = voices_in_part_in_parts (parts)

    all_ly_voices = {}
    for p, name_voice in all_voices.items ():

        part_ly_voices = {}
        for n, v in name_voice.items ():
            progress ("Converting to LilyPond expressions...")
            part_ly_voices[n] = (musicxml_voice_to_lily_voice (v), v)

        all_ly_voices[p] = part_ly_voices
        
    return all_ly_voices


def option_parser ():
    p = ly.get_option_parser(usage=_ ("musicxml2ly FILE.xml"),
                             version=('''%prog (LilyPond) @TOPLEVEL_VERSION@\n\n'''
                                      +
_ ("""This program is free software.  It is covered by the GNU General Public
License and you are welcome to change it and/or distribute copies of it
under certain conditions.  Invoke as `%s --warranty' for more
information.""") % 'lilypond'
+ """
Copyright (c) 2005--2007 by
    Han-Wen Nienhuys <hanwen@xs4all.nl> and
    Jan Nieuwenhuizen <janneke@gnu.org>
"""),
                             description=_ ("Convert %s to LilyPond input.") % 'MusicXML' + "\n")
    p.add_option ('-v', '--verbose',
                  action="store_true",
                  dest='verbose',
                  help=_ ("be verbose"))

    p.add_option ('', '--lxml',
                  action="store_true",
                  default=False,
                  dest="use_lxml",
                  help=_ ("Use lxml.etree; uses less memory and cpu time."))
    
    p.add_option ('-o', '--output',
                  metavar=_ ("FILE"),
                  action="store",
                  default=None,
                  type='string',
                  dest='output_name',
                  help=_ ("set output filename to FILE"))
    p.add_option_group ('bugs',
                        description=(_ ("Report bugs via")
                                     + ''' http://post.gmane.org/post.php'''
                                     '''?group=gmane.comp.gnu.lilypond.bugs\n'''))
    return p

def music_xml_voice_name_to_lily_name (part, name):
    str = "Part%sVoice%s" % (part.id, name)
    return musicxml_id_to_lily (str) 

def print_voice_definitions (printer, part_list, voices):
    part_dict={}
    for (part, nv_dict) in voices.items():
        part_dict[part.id] = (part, nv_dict)

    for part in part_list:
        (part, nv_dict) = part_dict.get (part.id, (None, {}))
        for (name, (voice, mxlvoice)) in nv_dict.items ():
            k = music_xml_voice_name_to_lily_name (part, name)
            printer.dump ('%s = ' % k)
            voice.print_ly (printer)
            printer.newline()

            
def uniq_list (l):
    return dict ([(elt,1) for elt in l]).keys ()
    
def print_score_setup (printer, part_list, voices):
    part_dict = dict ([(p.id, p) for p in voices.keys ()]) 

    printer ('<<')
    printer.newline ()
    for part_definition in part_list:
        part_name = part_definition.id
        part = part_dict.get (part_name)
        if not part:
            print 'unknown part in part-list:', part_name
            continue

        nv_dict = voices.get (part)
        staves = reduce (lambda x,y: x+ y,
                [mxlvoice._staves.keys ()
                 for (v, mxlvoice) in nv_dict.values ()],
                [])

        if len (staves) > 1:
            staves = uniq_list (staves)
            staves.sort ()
            printer ('\\context PianoStaff << ')
            printer.newline ()
            
            for s in staves:
                staff_voices = [music_xml_voice_name_to_lily_name (part, voice_name)
                        for (voice_name, (v, mxlvoice)) in nv_dict.items ()
                        if mxlvoice._start_staff == s]
                
                printer ('\\context Staff = "%s" << ' % s)
                printer.newline ()
                for v in staff_voices:
                    printer ('\\context Voice = "%s"  \\%s' % (v,v))
                    printer.newline ()
                printer ('>>')
                printer.newline ()
                
            printer ('>>')
            printer.newline ()
            
        else:
            printer ('\\new Staff <<')
            printer.newline ()
            for (n,v) in nv_dict.items ():

                n = music_xml_voice_name_to_lily_name (part, n) 
                printer ('\\context Voice = "%s"  \\%s' % (n,n))
            printer ('>>')
            printer.newline ()
            
    printer ('>>')
    printer.newline ()

def print_ly_preamble (printer, filename):
    printer.dump_version ()
    printer.print_verbatim ('%% automatically converted from %s\n' % filename)

def read_musicxml (filename, use_lxml):
    if use_lxml:
        import lxml.etree
        
        tree = lxml.etree.parse (filename)
        mxl_tree = musicxml.lxml_demarshal_node (tree.getroot ())
        return mxl_tree
    else:
        from xml.dom import minidom, Node
        
        doc = minidom.parse(filename)
        node = doc.documentElement
        return musicxml.minidom_demarshal_node (node)

    return None


def convert (filename, options):
    progress ("Reading MusicXML from %s ..." % filename)
    
    tree = read_musicxml (filename, options.use_lxml)

    part_list = []
    id_instrument_map = {}
    if tree.get_maybe_exist_typed_child (musicxml.Part_list):
        mxl_pl = tree.get_maybe_exist_typed_child (musicxml.Part_list)
        part_list = mxl_pl.get_named_children ("score-part")
        
    parts = tree.get_typed_children (musicxml.Part)
    voices = get_all_voices (parts)

    if not options.output_name:
        options.output_name = os.path.basename (filename) 
        options.output_name = os.path.splitext (options.output_name)[0]


    defs_ly_name = options.output_name + '-defs.ly'
    driver_ly_name = options.output_name + '.ly'

    printer = musicexp.Output_printer()
    progress ("Output to `%s'" % defs_ly_name)
    printer.set_file (open (defs_ly_name, 'w'))

    print_ly_preamble (printer, filename)
    print_voice_definitions (printer, part_list, voices)
    
    printer.close ()
    
    
    progress ("Output to `%s'" % driver_ly_name)
    printer = musicexp.Output_printer()
    printer.set_file (open (driver_ly_name, 'w'))
    print_ly_preamble (printer, filename)
    printer.dump (r'\include "%s"' % defs_ly_name)
    print_score_setup (printer, part_list, voices)
    printer.newline ()

    return voices


def main ():
    opt_parser = option_parser()

    (options, args) = opt_parser.parse_args ()
    if not args:
        opt_parser.print_usage()
        sys.exit (2)

    voices = convert (args[0], options)

if __name__ == '__main__':
    main()
