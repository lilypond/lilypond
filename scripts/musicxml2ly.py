#!@TARGET_PYTHON@

import optparse
import sys
import re
import os
import string
import codecs
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

def error_message (str):
    sys.stderr.write (str + '\n')
    sys.stderr.flush ()

# score information is contained in the <work>, <identification> or <movement-title> tags
# extract those into a hash, indexed by proper lilypond header attributes
def extract_score_information (tree):
    header = musicexp.Header ()
    def set_if_exists (field, value):
        if value:
            header.set_field (field, musicxml.escape_ly_output_string (value))

    work = tree.get_maybe_exist_named_child ('work')
    if work:
        set_if_exists ('title', work.get_work_title ())
        set_if_exists ('worknumber', work.get_work_number ())
        set_if_exists ('opus', work.get_opus ())
    else:
        movement_title = tree.get_maybe_exist_named_child ('movement-title')
        if movement_title:
            set_if_exists ('title', movement_title.get_text ())
    
    identifications = tree.get_named_children ('identification')
    for ids in identifications:
        set_if_exists ('copyright', ids.get_rights ())
        set_if_exists ('composer', ids.get_composer ())
        set_if_exists ('arranger', ids.get_arranger ())
        set_if_exists ('editor', ids.get_editor ())
        set_if_exists ('poet', ids.get_poet ())
            
        set_if_exists ('tagline', ids.get_encoding_software ())
        set_if_exists ('encodingsoftware', ids.get_encoding_software ())
        set_if_exists ('encodingdate', ids.get_encoding_date ())
        set_if_exists ('encoder', ids.get_encoding_person ())
        set_if_exists ('encodingdescription', ids.get_encoding_description ())

    return header


def extract_score_layout (part_list):
    layout = musicexp.StaffGroup (None)
    currentgroups_dict = {}
    currentgroups = []
    if not part_list:
        return layout

    def insert_into_layout (object):
            if len (currentgroups) > 0:
                group_to_insert = currentgroups_dict.get (currentgroups [-1], layout)
            else:
                group_to_insert = layout
            group_to_insert.appendStaff (object)
            return group_to_insert

    def read_score_part (el):
        if not isinstance (el, musicxml.Score_part):
            return
        staff = musicexp.Staff ()
        staff.id = el.id
        partname = el.get_maybe_exist_named_child ('part-name')
        # Finale gives unnamed parts the name "MusicXML Part" automatically!
        if partname and partname.get_text() != "MusicXML Part":
            staff.instrument_name = partname.get_text ()
        if el.get_maybe_exist_named_child ('part-abbreviation'):
            staff.short_instrument_name = el.get_maybe_exist_named_child ('part-abbreviation').get_text ()
        # TODO: Read in the MIDI device / instrument
        return staff


    parts_groups = part_list.get_all_children ()
    # the start/end group tags are not necessarily ordered correctly, so
    # we can't go through the children sequentially!

    if len (parts_groups) == 1 and isinstance (parts_group[1], musicxml.Score_part):
        return read_score_part (parts_group[1])

    for el in parts_groups:
        if isinstance (el, musicxml.Score_part):
            staff = read_score_part (el)
            insert_into_layout (staff)
        elif isinstance (el, musicxml.Part_group):
            if el.type == "start":
                group = musicexp.StaffGroup ()
                staff_group = insert_into_layout (group)
                # If we're inserting a nested staffgroup, we need to use InnerStaffGroup
                if staff_group != layout:
                    group.stafftype = "InnerStaffGroup"
                if hasattr (el, 'number'):
                    id = el.number
                    group.id = id
                    currentgroups_dict[id] = group
                    currentgroups.append (id)
                if el.get_maybe_exist_named_child ('group-name'):
                    group.instrument_name = el.get_maybe_exist_named_child ('group-name').get_text ()
                if el.get_maybe_exist_named_child ('group-abbreviation'):
                    group.short_instrument_name = el.get_maybe_exist_named_child ('group-abbreviation').get_text ()
                if el.get_maybe_exist_named_child ('group-symbol'):
                    group.symbol = el.get_maybe_exist_named_child ('group-symbol').get_text ()
                if el.get_maybe_exist_named_child ('group-barline'):
                    group.spanbar = el.get_maybe_exist_named_child ('group-barline').get_text ()

            elif el.type == "stop":
                # end the part-group, i.e. simply remove it from the lists
                if hasattr (el, 'number'):
                    pid = el.number
                elif len (currentgroups) > 0:
                    pid = el[-1]
                if pid:
                    del currentgroups_dict[pid]
                    currentgroups.remove (pid)
    return layout



def musicxml_duration_to_lily (mxl_note):
    d = musicexp.Duration ()
    # if the note has no Type child, then that method spits out a warning and 
    # returns 0, i.e. a whole note
    d.duration_log = mxl_note.get_duration_log ()

    d.dots = len (mxl_note.get_typed_children (musicxml.Dot))
    # Grace notes by specification have duration 0, so no time modification 
    # factor is possible. It even messes up the output with *0/1
    if not mxl_note.get_maybe_exist_typed_child (musicxml.Grace):
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
    (change.type, change.position, change.octave) = attributes.get_clef_information ()
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
        error_message ('unknown mode %s' % mode)

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
        if children:
            elts.append (func (attrs))
    
    return elts

spanner_event_dict = {
    'slur' : musicexp.SlurEvent,
    'beam' : musicexp.BeamEvent,
    'glissando' : musicexp.GlissandoEvent,
    'pedal' : musicexp.PedalEvent,
    'wavy-line' : musicexp.TrillSpanEvent,
    'octave-shift' : musicexp.OctaveShiftEvent,
    'wedge' : musicexp.HairpinEvent
}
spanner_type_dict = {
    'start': -1,
    'begin': -1,
    'crescendo': -1,
    'decreschendo': -1,
    'diminuendo': -1,
    'continue': 0,
    'up': -1,
    'down': -1,
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
        error_message ('unknown span event %s' % mxl_event)


    type = mxl_event.get_type ()
    span_direction = spanner_type_dict.get (type)
    # really check for None, because some types will be translated to 0, which
    # would otherwise also lead to the unknown span warning
    if span_direction != None:
        ev.span_direction = span_direction
    else:
        error_message ('unknown span type %s for %s' % (type, name))

    ev.set_span_type (type)
    ev.line_type = getattr (mxl_event, 'line-type', 'solid')

    # assign the size, which is used for octave-shift, etc.
    ev.size = mxl_event.get_size ()

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

def musicxml_tremolo_to_lily_event (mxl_event):
    if mxl_event.get_name () != "tremolo": 
        return
    ev = musicexp.TremoloEvent ()
    ev.bars = mxl_event.get_text ()
    return ev

def musicxml_bend_to_lily_event (mxl_event):
    if mxl_event.get_name () != "bend":
        return
    ev = musicexp.BendEvent ()
    ev.alter = mxl_event.bend_alter ()
    return ev


short_articulations_dict = {
  "staccato": ".",
  "tenuto": "-",
  "stopped": "+",
  "staccatissimo": "|",
  "accent": ">",
  "strong-accent": "^",
  #"portato": "_", # does not exist in MusicXML
    #"fingering": "", # fingering is special cased, as get_text() will be the event's name
}
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
    "inverted-mordent": "prall",
    #"schleifer": "?" 
    ##### TECHNICALS
    "up-bow": "upbow", 
    "down-bow": "downbow", 
    "harmonic": "flageolet", 
    #"open-string": "", 
    #"thumb-position": "", 
    #"pluck": "", 
    #"double-tongue": "", 
    #"triple-tongue": "", 
    #"snap-pizzicato": "", 
    #"fret": "", 
    #"string": "", 
    #"hammer-on": "", 
    #"pull-off": "", 
    #"bend": "bendAfter #%s", # bend is special-cased, as we need to process the bend-alter subelement!
    #"tap": "", 
    #"heel": "", 
    #"toe": "", 
    #"fingernails": ""
    ##### ARTICULATIONS
    #"detached-legato": "", 
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
articulation_spanners = [ "wavy-line" ]

def musicxml_articulation_to_lily_event (mxl_event):
    # wavy-line elements are treated as trill spanners, not as articulation ornaments
    if mxl_event.get_name () in articulation_spanners:
        return musicxml_spanner_to_lily_event (mxl_event)

    # special case, because of the bend-alter subelement
    if mxl_event.get_name() == "bend":
        return musicxml_bend_to_lily_event (mxl_event)

    # If we can write a shorthand, use them!
    if mxl_event.get_name() == "fingering":
        ev = musicexp.ShortArticulationEvent ()
        tp = mxl_event.get_text()
    # In all other cases, use the dicts to translate the xml tag name to a proper lilypond command
    elif short_articulations_dict.get (mxl_event.get_name ()):
        ev = musicexp.ShortArticulationEvent ()
        tp = short_articulations_dict.get (mxl_event.get_name ())
    else:
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
    # \breathe cannot have any direction modifier (^, _, -)!
    if dir and tp != "breathe":
        ev.force_direction = dir
    return ev


def musicxml_dynamics_to_lily_event (dynentry):
    dynamics_available = ( "p", "pp", "ppp", "pppp", "ppppp", "pppppp",
        "f", "ff", "fff", "ffff", "fffff", "ffffff",
        "mp", "mf", "sf", "sfp", "sfpp", "fp",
        "rf", "rfz", "sfz", "sffz", "fz" )
    if not dynentry.get_name() in dynamics_available:
        return
    event = musicexp.DynamicsEvent ()
    event.type = dynentry.get_name ()
    return event


direction_spanners = [ 'octave-shift', 'pedal', 'wedge' ]

def musicxml_direction_to_lily (n):
    # TODO: Handle the <staff> element!
    res = []
    dirtype_children = []
    for dt in n.get_typed_children (musicxml.DirType):
        dirtype_children += dt.get_all_children ()

    for entry in dirtype_children:
        if entry.get_name () == "dynamics":
            for dynentry in entry.get_all_children ():
                ev = musicxml_dynamics_to_lily_event (dynentry)
                if ev:
                    res.append (ev)

        # octave shifts. pedal marks, hairpins etc. are spanners:
        if entry.get_name() in direction_spanners:
            event = musicxml_spanner_to_lily_event (entry)
            if event:
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
        # rests can have display-octave and display-step, which are
        # treated like an ordinary note pitch
        rest = n.get_maybe_exist_typed_child (musicxml.Rest)
        event = musicexp.RestEvent()
        pitch = musicxml_restdisplay_to_lily (rest)
        event.pitch = pitch
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


## TODO
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

    def set_duration (self, duration):
        self.end_moment = self.begin_moment + duration
    def current_duration (self):
        return self.end_moment - self.begin_moment
        
    def add_music (self, music, duration):
        assert isinstance (music, musicexp.Music)
        if self.pending_multibar > Rational (0):
            self._insert_multibar ()

        self.elements.append (music)
        self.begin_moment = self.end_moment
        self.set_duration (duration)
        
        # Insert all pending dynamics right after the note/rest:
        if duration > Rational (0):
            for d in self.pending_dynamics:
                self.elements.append (d)
            self.pending_dynamics = []

    # Insert some music command that does not affect the position in the measure
    def add_command (self, command):
        assert isinstance (command, musicexp.Music)
        if self.pending_multibar > Rational (0):
            self._insert_multibar ()
        self.elements.append (command)

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
            error_message ('Negative skip %s' % diff)
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

        # if the position matches, find the last EventChord, do not cross a bar line!
        at = len( self.elements ) - 1
        while (at >= 0 and
               not isinstance (self.elements[at], musicexp.EventChord) and
               not isinstance (self.elements[at], musicexp.BarCheck)):
            at -= 1

        if (self.elements
            and at >= 0
            and isinstance (self.elements[at], musicexp.EventChord)
            and self.begin_moment == starting_at):
            value = self.elements[at]
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
    lyrics = {}
        
    for k in voice.get_lyrics_numbers ():
        lyrics[k] = []

    voice_builder = LilyPondVoiceBuilder()

    for n in voice._elements:
        if n.get_name () == 'forward':
            continue

        if isinstance (n, musicxml.Direction):
            for a in musicxml_direction_to_lily (n):
                if a.wait_for_note ():
                    voice_builder.add_dynamics (a)
                else:
                    voice_builder.add_command (a)
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
            error_message ('not a Note or Attributes? %s' % n)
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

        grace = n.get_maybe_exist_typed_child (musicxml.Grace)
        if grace:
            grace_chord = None
            if n.get_maybe_exist_typed_child (musicxml.Chord) and ev_chord.grace_elements:
                grace_chord = ev_chord.grace_elements.get_last_event_chord ()
            if not grace_chord:
                grace_chord = musicexp.EventChord ()
                ev_chord.append_grace (grace_chord)
            if hasattr (grace, 'slash'):
                # TODO: use grace_type = "appoggiatura" for slurred grace notes
                if grace.slash == "yes":
                    ev_chord.grace_type = "acciaccatura"
                elif grace.slash == "no":
                    ev_chord.grace_type = "grace"
            # now that we have inserted the chord into the grace music, insert
            # everything into that chord instead of the ev_chord
            ev_chord = grace_chord
            ev_chord.append (main_event)
        else:
            ev_chord.append (main_event)
            # When a note/chord has grace notes (duration==0), the duration of the
            # event chord is not yet known, but the event chord was already added
            # with duration 0. The following correct this when we hit the real note!
            if voice_builder.current_duration () == 0 and n._duration > 0:
                voice_builder.set_duration (n._duration)
        
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
                    error_message ('more than 1 slur?')

                lily_ev = musicxml_spanner_to_lily_event (slurs[0])
                ev_chord.append (lily_ev)

            mxl_tie = notations.get_tie ()
            if mxl_tie and mxl_tie.type == 'start':
                ev_chord.append (musicexp.TieEvent ())
                
            fermatas = notations.get_named_children ('fermata')
            for a in fermatas:
                ev = musicxml_fermata_to_lily_event (a)
                if ev: 
                    ev_chord.append (ev)

            arpeggiate = notations.get_named_children ('arpeggiate')
            for a in arpeggiate:
                ev_chord.append (musicexp.ArpeggioEvent ())

            glissandos = notations.get_named_children ('glissando')
            for a in glissandos:
                ev = musicxml_spanner_to_lily_event (a)
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

        # Extract the lyrics
        if not rest:
            note_lyrics_processed = []
            note_lyrics_elements = n.get_typed_children (musicxml.Lyric)
            for l in note_lyrics_elements:
                if l.get_number () < 0:
                    for k in lyrics.keys ():
                        lyrics[k].append (l.lyric_to_text ())
                        note_lyrics_processed.append (k)
                else:
                    lyrics[l.number].append(l.lyric_to_text ())
                    note_lyrics_processed.append (l.number)
            for lnr in lyrics.keys ():
                if not lnr in note_lyrics_processed:
                    lyrics[lnr].append ("\skip4")


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

    seq_music = musicexp.SequentialMusic ()

    if 'drummode' in modes_found.keys ():
        ## \key <pitch> barfs in drummode.
        ly_voice = [e for e in ly_voice
                    if not isinstance(e, musicexp.KeySignatureChange)]
    
    seq_music.elements = ly_voice
    lyrics_dict = {}
    for k in lyrics.keys ():
        lyrics_dict[k] = musicexp.Lyrics ()
        lyrics_dict[k].lyrics_syllables = lyrics[k]
    
    
    if len (modes_found) > 1:
       error_message ('Too many modes found %s' % modes_found.keys ())

    return_value = seq_music
    for mode in modes_found.keys ():
        v = musicexp.ModeChangingMusicWrapper()
        v.element = return_value
        v.mode = mode
        return_value = v
    
    return (return_value, lyrics_dict)


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

def musicxml_restdisplay_to_lily (mxl_rest):
    p = None
    step = mxl_rest.get_step ()
    if step:
        p = musicexp.Pitch()
        p.step = (ord (step) - ord ('A') + 7 - 2) % 7
    octave = mxl_rest.get_octave ()
    if octave and p:
        p.octave = octave - 4
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
            # musicxml_voice_to_lily_voice returns (lily_voice, {nr->lyrics, nr->lyrics})
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

def music_xml_lyrics_name_to_lily_name (part, name, lyricsnr):
    str = "Part%sVoice%sLyrics%s" % (part.id, name, lyricsnr)
    return musicxml_id_to_lily (str) 

def print_voice_definitions (printer, part_list, voices):
    part_dict={}
    for (part, nv_dict) in voices.items():
        part_dict[part.id] = (part, nv_dict)

    for part in part_list:
        (p, nv_dict) = part_dict.get (part.id, (None, {}))
        for (name, ((voice, lyrics), mxlvoice)) in nv_dict.items ():
            k = music_xml_voice_name_to_lily_name (p, name)
            printer.dump ('%s = ' % k)
            voice.print_ly (printer)
            printer.newline()
            
            for l in lyrics.keys ():
                lname = music_xml_lyrics_name_to_lily_name (p, name, l)
                printer.dump ('%s = ' %lname )
                lyrics[l].print_ly (printer)
                printer.newline()

            
def uniq_list (l):
    return dict ([(elt,1) for elt in l]).keys ()

# format the information about the staff in the form 
#     [staffid,
#         [
#            [voiceid1, [lyricsid11, lyricsid12,...] ...],
#            [voiceid2, [lyricsid21, lyricsid22,...] ...],
#            ...
#         ]
#     ]
# raw_voices is of the form [(voicename, lyrics)*]
def format_staff_info (part, staff_id, raw_voices):
    voices = []
    for (v, lyrics) in raw_voices:
        voice_name = music_xml_voice_name_to_lily_name (part, v)
        voice_lyrics = [music_xml_lyrics_name_to_lily_name (part, v, l)
                   for l in lyrics.keys ()]
        voices.append ([voice_name, voice_lyrics])
    return [staff_id, voices]

def update_score_setup (score_structure, part_list, voices):
    part_dict = dict ([(p.id, p) for p in voices.keys ()])
    final_part_dict = {}

    for part_definition in part_list:
        part_name = part_definition.id
        part = part_dict.get (part_name)
        if not part:
            error_message ('unknown part in part-list: %s' % part_name)
            continue

        nv_dict = voices.get (part)
        staves = reduce (lambda x,y: x+ y,
                [mxlvoice._staves.keys ()
                 for (v, mxlvoice) in nv_dict.values ()],
                [])
        staves_info = []
        if len (staves) > 1:
            staves_info = []
            staves = uniq_list (staves)
            staves.sort ()
            for s in staves:
                thisstaff_raw_voices = [(voice_name, lyrics) 
                    for (voice_name, ((music, lyrics), mxlvoice)) in nv_dict.items ()
                    if mxlvoice._start_staff == s]
                staves_info.append (format_staff_info (part, s, thisstaff_raw_voices))
        else:
            thisstaff_raw_voices = [(voice_name, lyrics) 
                for (voice_name, ((music, lyrics), mxlvoice)) in nv_dict.items ()]
            staves_info.append (format_staff_info (part, None, thisstaff_raw_voices))
        score_structure.setPartInformation (part_name, staves_info)

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

    score_structure = None
    mxl_pl = tree.get_maybe_exist_typed_child (musicxml.Part_list)
    if mxl_pl:
        score_structure = extract_score_layout (mxl_pl)
        part_list = mxl_pl.get_named_children ("score-part")

    # score information is contained in the <work>, <identification> or <movement-title> tags
    score_information = extract_score_information (tree)
    parts = tree.get_typed_children (musicxml.Part)
    voices = get_all_voices (parts)
    update_score_setup (score_structure, part_list, voices)

    if not options.output_name:
        options.output_name = os.path.basename (filename) 
        options.output_name = os.path.splitext (options.output_name)[0]
    elif re.match (".*\.ly", options.output_name):
        options.output_name = os.path.splitext (options.output_name)[0]


    defs_ly_name = options.output_name + '-defs.ly'
    driver_ly_name = options.output_name + '.ly'

    printer = musicexp.Output_printer()
    progress ("Output to `%s'" % defs_ly_name)
    printer.set_file (codecs.open (defs_ly_name, 'wb', encoding='utf-8'))

    print_ly_preamble (printer, filename)
    score_information.print_ly (printer)
    print_voice_definitions (printer, part_list, voices)
    
    printer.close ()
    
    
    progress ("Output to `%s'" % driver_ly_name)
    printer = musicexp.Output_printer()
    printer.set_file (codecs.open (driver_ly_name, 'wb', encoding='utf-8'))
    print_ly_preamble (printer, filename)
    printer.dump (r'\include "%s"' % os.path.basename (defs_ly_name))
    score_structure.print_ly (printer)
    printer.newline ()

    return voices

def get_existing_filename_with_extension (filename, ext):
    if os.path.exists (filename):
        return filename
    newfilename = filename + ".xml"
    if os.path.exists (newfilename):
        return newfilename;
    newfilename = filename + "xml"
    if os.path.exists (newfilename):
        return newfilename;
    return ''

def main ():
    opt_parser = option_parser()

    (options, args) = opt_parser.parse_args ()
    if not args:
        opt_parser.print_usage()
        sys.exit (2)
    
    # Allow the user to leave out the .xml or xml on the filename
    filename = get_existing_filename_with_extension (args[0], "xml")
    if filename and os.path.exists (filename):
        voices = convert (filename, options)
    else:
        progress ("Unable to find input file %s" % args[0])

if __name__ == '__main__':
    main()
