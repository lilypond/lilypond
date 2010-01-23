#!@TARGET_PYTHON@
#
# midi2ly.py -- LilyPond midi import script

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2010  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                 Jan Nieuwenhuizen <janneke@gnu.org>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


'''
TODO:
    * test on weird and unquantised midi input (lily-devel)
    * update doc and manpage

    * simply insert clef changes whenever too many ledger lines
        [to avoid tex capacity exceeded]
    * do not ever quant skips
    * better lyrics handling
    * [see if it is feasible to] move ly-classes to library for use in
        other converters, while leaving midi specific stuff here
'''

import os
import sys

"""
@relocate-preamble@
"""

import midi
import lilylib as ly
global _;_=ly._

################################################################
## CONSTANTS


LINE_BELL = 60
scale_steps = [0, 2, 4, 5, 7, 9, 11]
global_options = None

clocks_per_1 = 1536
clocks_per_4 = 0

time = 0
reference_note = 0
start_quant_clocks = 0

duration_quant_clocks = 0
allowed_tuplet_clocks = []


################################################################


program_name = sys.argv[0]
program_version = '@TOPLEVEL_VERSION@'

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')

errorport = sys.stderr

def identify ():
    sys.stdout.write ('%s (GNU LilyPond) %s\n' % (program_name, program_version))

def warranty ():
    identify ()
    ly.encoded_write (sys.stdout, '''
%s

  %s

%s
%s
''' % ( _ ('Copyright (c) %s by') % '2001--2010',
        '\n  '.join (authors),
        _ ('Distributed under terms of the GNU General Public License.'),
        _ ('It comes with NO WARRANTY.')))

def progress (s):
    ly.encoded_write (errorport, s + '\n')

def warning (s):
    progress (_ ("warning: ") + s)
        
def error (s):
    progress (_ ("error: ") + s)
    raise Exception (_ ("Exiting... "))

def system (cmd, ignore_error = 0):
    return ly.system (cmd, ignore_error=ignore_error)

def strip_extension (f, ext):
    (p, e) = os.path.splitext (f)
    if e == ext:
        e = ''
    return p + e


class Duration:
    allowed_durs = (1, 2, 4, 8, 16, 32, 64, 128)
    def __init__ (self, clocks):
        self.clocks = clocks
        if clocks <= 0:
            self.clocks = duration_quant_clocks
        (self.dur, self.num, self.den) = self.dur_num_den (clocks)
        
    def dur_num_den (self, clocks):
        for i in range (len (allowed_tuplet_clocks)):
            if clocks == allowed_tuplet_clocks[i]:
                return global_options.allowed_tuplets[i]

        dur = 0; num = 1; den = 1;
        g = gcd (clocks, clocks_per_1)
        if g:
            (dur, num) = (clocks_per_1 / g, clocks / g)
        if not dur in self.allowed_durs:
            dur = 4; num = clocks; den = clocks_per_4
        return (dur, num, den)

    def dump (self):
        if self.den == 1:
            if self.num == 1:
                s = '%d' % self.dur
            elif self.num == 3 and self.dur != 1:
                s = '%d.' % (self.dur / 2)
            else:
                s = '%d*%d' % (self.dur, self.num)
        else:
            s = '%d*%d/%d' % (self.dur, self.num, self.den)
            
        global reference_note
        reference_note.duration = self

        return s

    def compare (self, other):
        return self.clocks - other.clocks

def sign (x):
    if x >= 0:
        return 1
    else:
        return -1

class Note:
    names = (0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6)
    alterations = (0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0)
    alteration_names = ('eses', 'es', '', 'is' , 'isis')
    def __init__ (self, clocks, pitch, velocity):
        self.pitch = pitch
        self.velocity = velocity
        # hmm
        self.clocks = clocks
        self.duration = Duration (clocks)
        (self.octave, self.notename, self.alteration) = self.o_n_a ()

    def o_n_a (self):
        # major scale: do-do
        # minor scale: la-la  (= + 5) '''

        n = self.names[(self.pitch) % 12]
        a = self.alterations[(self.pitch) % 12]

        if a and global_options.key.flats:
            a = - self.alterations[(self.pitch) % 12]
            n = (n - a) % 7

        #  By tradition, all scales now consist of a sequence
        #  of 7 notes each with a distinct name, from amongst
        #  a b c d e f g.  But, minor scales have a wide
        #  second interval at the top - the 'leading note' is
        #  sharped. (Why? it just works that way! Anything
        #  else doesn't sound as good and isn't as flexible at
        #  saying things. In medieval times, scales only had 6
        #  notes to avoid this problem - the hexachords.)

        #  So, the d minor scale is d e f g a b-flat c-sharp d
        #  - using d-flat for the leading note would skip the
        #  name c and duplicate the name d.  Why isn't c-sharp
        #  put in the key signature? Tradition. (It's also
        #  supposedly based on the Pythagorean theory of the
        #  cycle of fifths, but that really only applies to
        #  major scales...)  Anyway, g minor is g a b-flat c d
        #  e-flat f-sharp g, and all the other flat minor keys
        #  end up with a natural leading note. And there you
        #  have it.

        #  John Sankey <bf250@freenet.carleton.ca>
        #
        #  Let's also do a-minor: a b c d e f gis a
        #
        #  --jcn

        o = self.pitch / 12 - 4

        key = global_options.key
        if key.minor:
            # as -> gis
            if (key.sharps == 0 and key.flats == 0
                and n == 5 and a == -1):
                n = 4; a = 1
            # des -> cis
            elif key.flats == 1 and n == 1 and a == -1:
                n = 0; a = 1
            # ges -> fis
            elif key.flats == 2 and n == 4 and a == -1:
                n = 3; a = 1
            # g -> fisis
            elif key.sharps == 5 and n == 4 and a == 0:
                n = 3; a = 2
            # d -> cisis
            elif key.sharps == 6 and n == 1 and a == 0:
                n = 0; a = 2
            # a -> gisis
            elif key.sharps == 7 and n == 5 and a == 0:
                n = 4; a = 2

        # b -> ces
        if key.flats >= 6 and n == 6 and a == 0:
            n = 0; a = -1; o = o + 1
        # e -> fes
        if key.flats >= 7 and n == 2 and a == 0:
            n = 3; a = -1

        # f -> eis
        if key.sharps >= 3 and n == 3 and a == 0:
            n = 2; a = 1
        # c -> bis
        if key.sharps >= 4 and n == 0 and a == 0:
            n = 6; a = 1; o = o - 1

        return (o, n, a)
        
    def __repr__ (self):
        s = chr ((self.notename + 2)  % 7 + ord ('a'))
        return 'Note(%s %s)' % (s, self.duration.dump())

    def dump (self, dump_dur = 1):
        global reference_note
        s = chr ((self.notename + 2)  % 7 + ord ('a'))
        s = s + self.alteration_names[self.alteration + 2]
        if global_options.absolute_pitches:
            commas = self.octave
        else:
            delta = self.pitch - reference_note.pitch
            commas = sign (delta) * (abs (delta) / 12)
            if ((sign (delta) \
              * (self.notename - reference_note.notename) + 7) \
              % 7 >= 4) \
              or ((self.notename == reference_note.notename) \
                and (abs (delta) > 4) and (abs (delta) < 12)):
                commas = commas + sign (delta)
            
        if commas > 0:
            s = s + "'" * commas
        elif commas < 0:
            s = s + "," * -commas

        ## FIXME: compile fix --jcn
        if dump_dur and (global_options.explicit_durations \
         or self.duration.compare (reference_note.duration)):
            s = s + self.duration.dump ()

        reference_note = self
        
        # TODO: move space
        return s + ' '


class Time:
    def __init__ (self, num, den):
        self.clocks = 0
        self.num = num
        self.den = den

    def bar_clocks (self):
        return clocks_per_1 * self.num / self.den

    def __repr__ (self):
        return 'Time(%d/%d)' % (self.num, self.den)
    
    def dump (self):
        global time
        time = self
        return '\n  ' + '\\time %d/%d ' % (self.num, self.den) + '\n  '

class Tempo:
    def __init__ (self, seconds_per_1):
        self.clocks = 0
        self.seconds_per_1 = seconds_per_1

    def __repr__ (self):
        return 'Tempo(%d)' % self.bpm ()
    
    def bpm (self):
        return 4 * 60 / self.seconds_per_1
    
    def dump (self):
        return '\n  ' + '\\tempo 4 = %d ' % (self.bpm()) + '\n  '

class Clef:
    clefs = ('"bass_8"', 'bass', 'violin', '"violin^8"')
    def __init__ (self, type):
        self.type = type

    def __repr__ (self):
        return 'Clef(%s)' % self.clefs[self.type]
    
    def dump (self):
        return '\n  \\clef %s\n  ' % self.clefs[self.type]

class Key:
    key_sharps = ('c', 'g', 'd', 'a', 'e', 'b', 'fis')
    key_flats = ('BUG', 'f', 'bes', 'es', 'as', 'des', 'ges')

    def __init__ (self, sharps, flats, minor):
        self.clocks = 0
        self.flats = flats
        self.sharps = sharps
        self.minor = minor

    def dump (self):
        global_options.key = self

        s = ''
        if self.sharps and self.flats:
            pass
        else:
            if self.flats:
                k = (ord ('cfbeadg'[self.flats % 7]) - ord ('a') - 2 -2 * self.minor + 7) % 7
            else:
                k = (ord ('cgdaebf'[self.sharps % 7]) - ord ('a') - 2 -2 * self.minor + 7) % 7
 
            if not self.minor:
                name = chr ((k + 2) % 7 + ord ('a'))
            else:
                name = chr ((k + 2) % 7 + ord ('a'))

            # fis cis gis dis ais eis bis
            sharps = (2, 4, 6, 1, 3, 5, 7)
            # bes es as des ges ces fes
            flats = (6, 4, 2, 7, 5, 3, 1)
            a = 0
            if self.flats:
                if flats[k] <= self.flats:
                    a = -1
            else:
                if sharps[k] <= self.sharps:
                    a = 1

            if a:
                name = name + Note.alteration_names[a + 2]

            s = '\\key ' + name
            if self.minor:
                s = s + ' \\minor'
            else:
                s = s + ' \\major'

        return '\n\n  ' + s + '\n  '


class Text:
    text_types = (
        'SEQUENCE_NUMBER',
        'TEXT_EVENT',
        'COPYRIGHT_NOTICE',
        'SEQUENCE_TRACK_NAME',
        'INSTRUMENT_NAME',
        'LYRIC',
        'MARKER',
        'CUE_POINT',)
    
    def __init__ (self, type, text):
        self.clocks = 0
        self.type = type
        self.text = text

    def dump (self):
        # urg, we should be sure that we're in a lyrics staff
        if self.type == midi.LYRIC:
            s = '"%s"' % self.text
            d = Duration (self.clocks)
            if global_options.explicit_durations \
             or d.compare (reference_note.duration):
                s = s + Duration (self.clocks).dump ()
            s = s + ' '
        else:
            s = '\n  % [' + self.text_types[self.type] + '] ' + self.text + '\n  '
        return s

    def __repr__ (self):
        return 'Text(%d=%s)' % (self.type, self.text)



def split_track (track):
    chs = {}
    for i in range(16):
        chs[i] = []
        
    for e in track:
        data = list (e[1])
        if data[0] > 0x7f and data[0] < 0xf0:
            c = data[0] & 0x0f
            e = (e[0], tuple ([data[0] & 0xf0] + data[1:]))
            chs[c].append (e)
        else:
            chs[0].append (e)

    for i in range (16):
        if chs[i] == []:
            del chs[i]

    threads = []
    for v in chs.values ():
        events = events_on_channel (v)
        thread = unthread_notes (events)
        if len (thread):
            threads.append (thread)
    return threads


def quantise_clocks (clocks, quant):
    q = int (clocks / quant) * quant
    if q != clocks:
        for tquant in allowed_tuplet_clocks:
            if int (clocks / tquant) * tquant == clocks:
                return clocks
        if 2 * (clocks - q) > quant:
            q = q + quant
    return q

def end_note (pitches, notes, t, e):
    try:
        (lt, vel) = pitches[e]
        del pitches[e]

        i = len (notes) - 1 
        while i > 0:
            if notes[i][0] > lt:
                i = i -1
            else:
                break
        d = t - lt
        if duration_quant_clocks:
            d = quantise_clocks (d, duration_quant_clocks)
            if not d:
                d = duration_quant_clocks

        notes.insert (i + 1,
              (lt, Note (d, e, vel)))

    except KeyError:
        pass

def events_on_channel (channel):
    pitches = {}

    notes = []
    events = []
    last_lyric = 0
    last_time = 0
    for e in channel:
        t = e[0]

        if start_quant_clocks:
            t = quantise_clocks (t, start_quant_clocks)


        if e[1][0] == midi.NOTE_OFF \
         or (e[1][0] == midi.NOTE_ON and e[1][2] == 0):
            end_note (pitches, notes, t, e[1][1])
            
        elif e[1][0] == midi.NOTE_ON:
            if not pitches.has_key (e[1][1]):
                pitches[e[1][1]] = (t, e[1][2])
                
        # all include ALL_NOTES_OFF
        elif e[1][0] >= midi.ALL_SOUND_OFF \
          and e[1][0] <= midi.POLY_MODE_ON:
            for i in pitches:
                end_note (pitches, notes, t, i)
                
        elif e[1][0] == midi.META_EVENT:
            if e[1][1] == midi.END_OF_TRACK:
                for i in pitches:
                    end_note (pitches, notes, t, i)
                break

            elif e[1][1] == midi.SET_TEMPO:
                (u0, u1, u2) = map (ord, e[1][2])
                us_per_4 = u2 + 256 * (u1 + 256 * u0)
                seconds_per_1 = us_per_4 * 4 / 1e6
                events.append ((t, Tempo (seconds_per_1)))
            elif e[1][1] == midi.TIME_SIGNATURE:
                (num, dur, clocks4, count32) = map (ord, e[1][2])
                den = 2 ** dur 
                events.append ((t, Time (num, den)))
            elif e[1][1] == midi.KEY_SIGNATURE:
                (alterations, minor) = map (ord, e[1][2])
                sharps = 0
                flats = 0
                if alterations < 127:
                    sharps = alterations
                else:
                    flats = 256 - alterations

                k = Key (sharps, flats, minor)
                events.append ((t, k))

                # ugh, must set key while parsing
                # because Note init uses key
                # Better do Note.calc () at dump time?
                global_options.key = k

            elif e[1][1] == midi.LYRIC \
              or (global_options.text_lyrics and e[1][1] == midi.TEXT_EVENT):
                if last_lyric:
                    last_lyric.clocks = t - last_time
                    events.append ((last_time, last_lyric))
                last_time = t
                last_lyric = Text (midi.LYRIC, e[1][2])

            elif e[1][1] >= midi.SEQUENCE_NUMBER \
              and e[1][1] <= midi.CUE_POINT:
                events.append ((t, Text (e[1][1], e[1][2])))
            else:
                if global_options.verbose:
                    sys.stderr.write ("SKIP: %s\n" % `e`)
                pass
        else:
            if global_options.verbose:
                sys.stderr.write ("SKIP: %s\n" % `e`)
            pass

    if last_lyric:
        # last_lyric.clocks = t - last_time
        # hmm
        last_lyric.clocks = clocks_per_4
        events.append ((last_time, last_lyric))
        last_lyric = 0
        
    i = 0
    while len (notes):
        if i < len (events) and notes[0][0] >= events[i][0]:
            i = i + 1
        else:
            events.insert (i, notes[0])
            del notes[0]
    return events

def unthread_notes (channel):
    threads = []
    while channel:
        thread = []
        end_busy_t = 0
        start_busy_t = 0
        todo = []
        for e in channel:
            t = e[0]
            if e[1].__class__ == Note \
             and ((t == start_busy_t \
                and e[1].clocks + t == end_busy_t) \
              or t >= end_busy_t):
                thread.append (e)
                start_busy_t = t
                end_busy_t = t + e[1].clocks
            elif e[1].__class__ == Time \
              or e[1].__class__ == Key \
              or e[1].__class__ == Text \
              or e[1].__class__ == Tempo:
                thread.append (e)
            else:
                todo.append (e)
        threads.append (thread)
        channel = todo

    return threads

def gcd (a,b):
    if b == 0:
        return a
    c = a
    while c: 
        c = a % b
        a = b
        b = c
    return a
    
def dump_skip (skip, clocks):
    return skip + Duration (clocks).dump () + ' '

def dump (d):
    return d.dump ()

def dump_chord (ch):
    s = ''
    notes = []
    for i in ch:
        if i.__class__ == Note:
            notes.append (i)
        else:
            s = s + i.dump ()
    if len (notes) == 1:
        s = s + dump (notes[0])
    elif len (notes) > 1:
        global reference_note
        s = s + '<'
        s = s + notes[0].dump (dump_dur = 0)
        r = reference_note
        for i in notes[1:]:
            s = s + i.dump (dump_dur = 0 )
        s = s + '>'

        s = s + notes[0].duration.dump() + ' '
        reference_note = r
    return s

def dump_bar_line (last_bar_t, t, bar_count):
    s = ''
    bar_t = time.bar_clocks ()
    if t - last_bar_t >= bar_t:
        bar_count = bar_count + (t - last_bar_t) / bar_t
        
        if t - last_bar_t == bar_t:
            s = '|\n  %% %d\n  ' % bar_count
            last_bar_t = t
        else:
            # urg, this will barf at meter changes
            last_bar_t = last_bar_t + (t - last_bar_t) / bar_t * bar_t
            
    return (s, last_bar_t, bar_count)

            
def dump_channel (thread, skip):
    global reference_note, time

    global_options.key = Key (0, 0, 0)
    time = Time (4, 4)
    # urg LilyPond doesn't start at c4, but
    # remembers from previous tracks!
    # reference_note = Note (clocks_per_4, 4*12, 0)
    reference_note = Note (0, 4*12, 0)
    last_e = None
    chs = []
    ch = []

    for e in thread:
        if last_e and last_e[0] == e[0]:
            ch.append (e[1])
        else:
            if ch:
                chs.append ((last_e[0], ch))
                
            ch = [e[1]]
            
        last_e = e

    if ch:
        chs.append ((last_e[0], ch))
    t = 0
    last_t = 0
    last_bar_t = 0
    bar_count = 1
    
    lines = ['']
    for ch in chs: 
        t = ch[0]

        i = lines[-1].rfind ('\n') + 1
        if len (lines[-1][i:]) > LINE_BELL:
            lines.append ('')

        if t - last_t > 0:
            lines[-1] = lines[-1] + dump_skip (skip, t-last_t)
        elif t - last_t < 0:
            errorport.write ('BUG: time skew')

        (s, last_bar_t, bar_count) = dump_bar_line (last_bar_t,
                              t, bar_count)
        lines[-1] = lines[-1] + s
        
        lines[-1] = lines[-1] + dump_chord (ch[1])

        clocks = 0
        for i in ch[1]:
            if i.clocks > clocks:
                clocks = i.clocks
                
        last_t = t + clocks
        
        (s, last_bar_t, bar_count) = dump_bar_line (last_bar_t,
                              last_t, bar_count)
        lines[-1] = lines[-1] + s

    return '\n  '.join (lines) + '\n'

def track_name (i):
    return 'track%c' % (i + ord ('A'))

def channel_name (i):
    return 'channel%c' % (i + ord ('A'))

def dump_track (channels, n):
    s = '\n'
    track = track_name (n)
    clef = guess_clef (channels)

    for i in range (len (channels)):
        channel = channel_name (i)
        item = thread_first_item (channels[i])

        if item and item.__class__ == Note:
            skip = 's'
            s = s + '%s = ' % (track + channel)
            if not global_options.absolute_pitches:
                s = s + '\\relative c '
        elif item and item.__class__ == Text:
            skip = '" "'
            s = s + '%s = \\lyricmode ' % (track + channel)
        else:
            skip = '\\skip '
            s = s + '%s =  ' % (track + channel)
        s = s + '{\n'
        s = s + '  ' + dump_channel (channels[i][0], skip)
        s = s + '}\n\n'

    s = s + '%s = <<\n' % track

    if clef.type != 2:
        s = s + clef.dump () + '\n'

    for i in range (len (channels)):
        channel = channel_name (i)
        item = thread_first_item (channels[i])
        if item and item.__class__ == Text:
            s = s + '  \\context Lyrics = %s \\%s\n' % (channel,
                                  track + channel)
        else:
            s = s + '  \\context Voice = %s \\%s\n' % (channel,
                                 track + channel)
    s = s + '>>\n\n'
    return s

def thread_first_item (thread):
    for chord in thread:
        for event in chord:
            if (event[1].__class__ == Note 
              or (event[1].__class__ == Text 
                and event[1].type == midi.LYRIC)):
                
              return event[1]
    return None

def track_first_item (track):
    for thread in track:
        first = thread_first_item (thread)
        if first:
            return first
    return None

def guess_clef (track):
    i = 0
    p = 0
    for thread in track:
        for chord in thread:
            for event in chord:
                if event[1].__class__ == Note:
                    i = i + 1
                    p = p + event[1].pitch
    if i and p / i <= 3*12:
        return Clef (0)
    elif i and p / i <= 5*12:
        return Clef (1)
    elif i and p / i >= 7*12:
        return Clef (3)
    else:
        return Clef (2)
    

def convert_midi (in_file, out_file):
    global clocks_per_1, clocks_per_4, key
    global start_quant_clocks
    global  duration_quant_clocks
    global allowed_tuplet_clocks

    str = open (in_file).read ()
    midi_dump = midi.parse (str)
    
    clocks_per_1 = midi_dump[0][1]
    clocks_per_4 = clocks_per_1 / 4
    
    if global_options.start_quant:
        start_quant_clocks = clocks_per_1 / global_options.start_quant

    if global_options.duration_quant:
        duration_quant_clocks = clocks_per_1 / global_options.duration_quant

    allowed_tuplet_clocks = []
    for (dur, num, den) in global_options.allowed_tuplets:
        allowed_tuplet_clocks.append (clocks_per_1 / den)

    tracks = []
    for t in midi_dump[1]:
        global_options.key = Key (0, 0, 0)
        tracks.append (split_track (t))

    tag = '%% Lily was here -- automatically converted by %s from %s' % ( program_name, in_file)

    
    s = ''
    s = tag + '\n\\version "2.7.18"\n\n'
    for i in range (len (tracks)):
        s = s + dump_track (tracks[i], i)

    s = s + '\n\\score {\n  <<\n'
    
    i = 0
    for t in tracks:
        track = track_name (i)
        item = track_first_item (t)
        
        if item and item.__class__ == Note:
            s = s + '    \\context Staff=%s \\%s\n' % (track, track)
        elif item and item.__class__ == Text:
            s = s + '    \\context Lyrics=%s \\%s\n' % (track, track)

        i += 1
    s = s + '  >>\n}\n'

    progress (_ ("%s output to `%s'...") % ('LY', out_file))

    if out_file == '-':
        handle = sys.stdout
    else:
        handle = open (out_file, 'w')

    handle.write (s)
    handle.close ()


def get_option_parser ():
    p = ly.get_option_parser (usage=_ ("%s [OPTION]... FILE") % 'midi2ly',
                 description=_ ("Convert %s to LilyPond input.\n") % 'MIDI',
                 add_help_option=False)

    p.add_option ('-a', '--absolute-pitches',
           action='store_true',
           help=_ ("print absolute pitches"))
    p.add_option ('-d', '--duration-quant',
           metavar= _("DUR"),
           help=_ ("quantise note durations on DUR"))
    p.add_option ('-e', '--explicit-durations',
           action='store_true',
           help=_ ("print explicit durations"))
    p.add_option("-h", "--help",
                 action="help",
                 help=_ ("show this help and exit"))
    p.add_option('-k', '--key', help=_ ("set key: ALT=+sharps|-flats; MINOR=1"),
          metavar=_ ("ALT[:MINOR]"),
          default='0'),
    p.add_option ('-o', '--output', help=_ ("write output to FILE"),
           metavar=_("FILE"),
           action='store')
    p.add_option ('-s', '--start-quant',help= _ ("quantise note starts on DUR"),
           metavar=_ ("DUR"))
    p.add_option ('-t', '--allow-tuplet',
           metavar=_ ("DUR*NUM/DEN"),
           action = "append",
           dest="allowed_tuplets",
           help=_ ("allow tuplet durations DUR*NUM/DEN"),
           default=[])
    p.add_option ('-V', '--verbose', help=_ ("be verbose"),
           action='store_true'
           ),
    p.version = "midi2ly (LilyPond) @TOPLEVEL_VERSION@"
    p.add_option("--version",
                 action="version",
                 help=_ ("show version number and exit"))
    p.add_option ('-w', '--warranty', help=_ ("show warranty and copyright"),
           action='store_true',
           ),
    p.add_option ('-x', '--text-lyrics', help=_ ("treat every text as a lyric"),
           action='store_true')

    p.add_option_group (ly.display_encode (_ ("Examples")),
              description = r'''
  $ midi2ly --key=-2:1 --duration-quant=32 --allow-tuplet=4*2/3 --allow-tuplet=2*4/3 foo.midi
''')
    p.add_option_group ('',
                        description=(
            _ ('Report bugs via %s')
            % 'http://post.gmane.org/post.php'
            '?group=gmane.comp.gnu.lilypond.bugs') + '\n')
    return p



def do_options ():
    opt_parser = get_option_parser()
    (options, args) = opt_parser.parse_args ()

    if not args or args[0] == '-':
        opt_parser.print_help ()
        ly.stderr_write ('\n%s: %s %s\n' % (program_name, _ ("error: "),
                         _ ("no files specified on command line.")))
        sys.exit (2)

    if options.duration_quant:
        options.duration_quant = int (options.duration_quant)

    if options.warranty:
        warranty ()
        sys.exit (0)
    if 1:
        (alterations, minor) = map (int, (options.key + ':0').split (':'))[0:2]
        sharps = 0
        flats = 0
        if alterations >= 0:
            sharps = alterations
        else:
            flats = - alterations

        options.key = Key (sharps, flats, minor)

        
    if options.start_quant:
        options.start_quant = int (options.start_quant)
        
    options.allowed_tuplets = [map (int, a.replace ('/','*').split ('*'))
                for a in options.allowed_tuplets]
    
    global global_options
    global_options = options

    return args

def main():
    files = do_options()

    for f in files:
        g = f
        g = strip_extension (g, '.midi')
        g = strip_extension (g, '.mid')
        g = strip_extension (g, '.MID')
        (outdir, outbase) = ('','')

        if not global_options.output:
            outdir = '.'
            outbase = os.path.basename (g)
            o = os.path.join (outdir, outbase + '-midi.ly')
        elif global_options.output[-1] == os.sep:
            outdir = global_options.output
            outbase = os.path.basename (g)
            os.path.join (outdir, outbase + '-gen.ly')
        else:
            o = global_options.output
            (outdir, outbase) = os.path.split (o)

        if outdir != '.' and outdir != '':
            try:
                os.mkdir (outdir, 0777)
            except OSError:
                pass

        convert_midi (f, o)
if __name__ == '__main__':
    main()
