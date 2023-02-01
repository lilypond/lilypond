#!@TARGET_PYTHON@
#
# midi2ly.py -- LilyPond midi import script

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
#                           Jan Nieuwenhuizen <janneke@gnu.org>
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
'''

import gettext
import math
import os
import sys

"""
@relocate-preamble@
"""

# Load translation and install _() into Python's builtins namespace.
gettext.install('lilypond', '@localedir@')

import lilylib as ly

################################################################
# CONSTANTS


LINE_BELL = 60
scale_steps = [0, 2, 4, 5, 7, 9, 11]
global_options = None

clocks_per_1 = 1536
clocks_per_4 = 0

time = None
reference_note = 0 # a mess
start_quant_clocks = 0

duration_quant_clocks = 0
allowed_tuplet_clocks = []
bar_max = 0

################################################################


program_version = '@TOPLEVEL_VERSION@'

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')



def identify():
    sys.stdout.write('%s (GNU LilyPond) %s\n' %
                     (ly.program_name, program_version))


def warranty():
    identify()
    sys.stdout.write('''
%s

  %s

%s
%s
''' % (_('Copyright (c) %s by') % '1998--2023',
        '\n  '.join(authors),
        _('Distributed under terms of the GNU General Public License.'),
        _('It comes with NO WARRANTY.')))


def strip_extension(f, ext):
    (p, e) = os.path.splitext(f)
    if e == ext:
        e = ''
    return p + e


class Duration:
    allowed_durs = (1, 2, 4, 8, 16, 32, 64, 128)

    def __init__(self, clocks):
        self.clocks = clocks
        (self.dur, self.num, self.den) = self.dur_num_den(clocks)

    def dur_num_den(self, clocks):
        for i in range(len(allowed_tuplet_clocks)):
            if clocks == allowed_tuplet_clocks[i]:
                return global_options.allowed_tuplets[i]

        dur = 0
        num = 1
        den = 1
        g = math.gcd(int(clocks), clocks_per_1)
        if g:
            (dur, num) = (clocks_per_1 / g, clocks / g)
        if not dur in self.allowed_durs:
            dur = 4
            num = clocks
            den = clocks_per_4
        return (dur, num, den)

    def __repr__(self):
        if self.den == 1:
            if self.num == 1:
                s = '%d' % self.dur
            elif self.num == 3 and self.dur != 1:
                s = '%d.' % (self.dur / 2)
            else:
                s = '%d*%d' % (self.dur, self.num)
        else:
            s = '%d*%d/%d' % (self.dur, self.num, self.den)
        return s

    def dump(self):
        global reference_note
        reference_note.duration = self
        return repr(self)

    def compare(self, other):
        return self.clocks - other.clocks


def sign(x):
    if x >= 0:
        return 1
    else:
        return -1


class Note:
    names = (0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6)
    alterations = (0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0)
    alteration_names = ('eses', 'es', '', 'is', 'isis')

    def __init__(self, clocks, pitch, velocity):
        self.pitch = pitch
        self.velocity = velocity
        # hmm
        self.clocks = clocks
        self.duration = Duration(clocks)
        (self.octave, self.notename, self.alteration) = self.o_n_a()

    def o_n_a(self):
        # major scale: do-do
        # minor scale: la-la  (= + 5) '''

        n = self.names[(self.pitch) % 12]
        a = self.alterations[(self.pitch) % 12]

        key = global_options.key
        if not key:
            key = Key(0, 0, 0)

        if a and key.flats:
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

        if key.minor:
            # as -> gis
            if (key.sharps == 0 and key.flats == 0
                    and n == 5 and a == -1):
                n = 4
                a = 1
            # des -> cis
            elif key.flats == 1 and n == 1 and a == -1:
                n = 0
                a = 1
            # ges -> fis
            elif key.flats == 2 and n == 4 and a == -1:
                n = 3
                a = 1
            # g -> fisis
            elif key.sharps == 5 and n == 4 and a == 0:
                n = 3
                a = 2
            # d -> cisis
            elif key.sharps == 6 and n == 1 and a == 0:
                n = 0
                a = 2
            # a -> gisis
            elif key.sharps == 7 and n == 5 and a == 0:
                n = 4
                a = 2

        # b -> ces
        if key.flats >= 6 and n == 6 and a == 0:
            n = 0
            a = -1
            o = o + 1
        # e -> fes
        if key.flats >= 7 and n == 2 and a == 0:
            n = 3
            a = -1

        # f -> eis
        if key.sharps >= 3 and n == 3 and a == 0:
            n = 2
            a = 1
        # c -> bis
        if key.sharps >= 4 and n == 0 and a == 0:
            n = 6
            a = 1
            o = o - 1

        return (o, n, a)

    def __repr__(self):
        s = chr((self.notename + 2) % 7 + ord('a'))
        return 'Note(%s %s)' % (s, repr(self.duration))

    def dump(self, dump_dur=True):
        global reference_note
        s = chr((self.notename + 2) % 7 + ord('a'))
        s = s + self.alteration_names[self.alteration + 2]
        if global_options.absolute_pitches:
            commas = self.octave
        else:
            delta = self.pitch - reference_note.pitch
            commas = sign(delta) * (abs(delta) // 12)
            if (((sign(delta)
                  * (self.notename - reference_note.notename) + 7)
                 % 7 >= 4)
                or ((self.notename == reference_note.notename)
                    and (abs(delta) > 4) and (abs(delta) < 12))):
                commas = commas + sign(delta)

        if commas > 0:
            s = s + "'" * commas
        elif commas < 0:
            s = s + "," * -commas

        if (dump_dur
            and (self.duration.compare(reference_note.duration)
                 or global_options.explicit_durations)):
            s = s + self.duration.dump()

        # Chords need to handle their reference duration themselves

        reference_note = self

        # TODO: move space
        return s + ' '


class Time:
    def __init__(self, num, den):
        self.clocks = 0
        self.num = num
        self.den = den

    def bar_clocks(self):
        return clocks_per_1 * self.num / self.den

    def __repr__(self):
        return 'Time(%d/%d)' % (self.num, self.den)

    def dump(self):
        global time
        time = self
        return '\n  ' + '\\time %d/%d ' % (self.num, self.den) + '\n  '


class Tempo:
    def __init__(self, seconds_per_1):
        self.clocks = 0
        self.seconds_per_1 = seconds_per_1

    def __repr__(self):
        return 'Tempo(%d)' % self.bpm()

    def bpm(self):
        return 4 * 60 / self.seconds_per_1

    def dump(self):
        return '\n  ' + '\\tempo 4 = %d ' % (self.bpm()) + '\n  '


class Clef:
    clefs = ('"bass_8"', 'bass', 'violin', '"violin^8"')

    def __init__(self, type):
        self.type = type

    def __repr__(self):
        return 'Clef(%s)' % self.clefs[self.type]

    def dump(self):
        return '\n  \\clef %s\n  ' % self.clefs[self.type]


class Key:
    key_sharps = ('c', 'g', 'd', 'a', 'e', 'b', 'fis')
    key_flats = ('BUG', 'f', 'bes', 'es', 'as', 'des', 'ges')

    def __init__(self, sharps, flats, minor):
        self.clocks = 0
        self.flats = flats
        self.sharps = sharps
        self.minor = minor

    def dump(self):
        global_options.key = self

        s = ''
        if self.sharps and self.flats:
            pass
        else:
            if self.flats:
                k = (ord('cfbeadg'[self.flats % 7]) -
                     ord('a') - 2 - 2 * self.minor + 7) % 7
            else:
                k = (ord('cgdaebf'[self.sharps % 7]) -
                     ord('a') - 2 - 2 * self.minor + 7) % 7

            if not self.minor:
                name = chr((k + 2) % 7 + ord('a'))
            else:
                name = chr((k + 2) % 7 + ord('a'))

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
        'CUE_POINT',
        'PROGRAM_NAME',
        'DEVICE_NAME', )

    @staticmethod
    def _text_only(chr):
        if ((' ' <= chr <= '~') or chr in ['\n', '\r']):
            return chr
        else:
            return '~'

    def __init__(self, type, text):
        self.clocks = 0
        self.type = type
        self.text = ''.join(map(self._text_only, text))

    def dump(self):
        # urg, we should be sure that we're in a lyrics staff
        s = ''
        if self.type == midi.LYRIC:
            s = '"%s"' % self.text
            d = Duration(self.clocks)
            if (global_options.explicit_durations
                    or d.compare(reference_note.duration)):
                s = s + Duration(self.clocks).dump()
            s = s + ' '
        elif (self.text.strip()
              and self.type == midi.SEQUENCE_TRACK_NAME
              and not self.text == 'control track'
              and not self.track.lyrics_p_):
            text = self.text.replace('(MIDI)', '').strip()
            if text:
                s = '\n  \\set Staff.instrumentName = "%(text)s"\n  ' % locals(
                )
        elif self.text.strip():
            s = '\n  % [' + self.text_types[self.type] + '] ' + \
                self.text + '\n  '
        return s

    def __repr__(self):
        return 'Text(%d=%s)' % (self.type, self.text)

class EndOfTrack:
    def __init__(self):
        self.clocks = 0

    def __repr__(self):
        return 'EndOfTrack()'

    def dump(self):
        return ''

def get_voice(channel, music):
    ly.debug_output('channel: ' + str(channel) + '\n')
    return unthread_notes(music)


class Channel:
    def __init__(self, number):
        self.number = number
        self.events = []
        self.music = None

    def add(self, event):
        self.events.append(event)

    def get_voice(self):
        if not self.music:
            self.music = self.parse()
        return get_voice(self.number, self.music)

    def parse(self):
        pitches = {}
        notes = []
        music = []
        last_lyric = 0
        last_time = 0
        end_of_track_time = None
        for e in self.events:
            t = e[0]

            if start_quant_clocks:
                t = quantise_clocks(t, start_quant_clocks)

            if (e[1][0] == midi.NOTE_OFF
                    or (e[1][0] == midi.NOTE_ON and e[1][2] == 0)):
                ly.debug_output('%d: NOTE OFF: %s' % (t, e[1][1]))
                if not e[1][2]:
                    ly.debug_output('   ...treated as OFF')
                end_note(pitches, notes, t, e[1][1])

            elif e[1][0] == midi.NOTE_ON:
                if e[1][1] not in pitches:
                    ly.debug_output('%d: NOTE ON: %s' % (t, e[1][1]))
                    pitches[e[1][1]] = (t, e[1][2])
                else:
                    ly.debug_output('...ignored')

            # all include ALL_NOTES_OFF
            elif (e[1][0] >= midi.ALL_SOUND_OFF
                  and e[1][0] <= midi.POLY_MODE_ON):
                for i in pitches:
                    end_note(pitches, notes, t, i)

            elif e[1][0] == midi.META_EVENT:
                if e[1][1] == midi.END_OF_TRACK:
                    for i in pitches:
                        end_note(pitches, notes, t, i)
                    end_of_track_time = t
                    break

                elif e[1][1] == midi.SET_TEMPO:
                    (u0, u1, u2) = list(map(ord, e[1][2]))
                    us_per_4 = u2 + 256 * (u1 + 256 * u0)
                    seconds_per_1 = us_per_4 * 4 / 1e6
                    music.append((t, Tempo(seconds_per_1)))
                elif e[1][1] == midi.TIME_SIGNATURE:
                    (num, dur, clocks4, count32) = list(map(ord, e[1][2]))
                    den = 2 ** dur
                    music.append((t, Time(num, den)))
                elif e[1][1] == midi.KEY_SIGNATURE:
                    (alterations, minor) = list(map(ord, e[1][2]))
                    sharps = 0
                    flats = 0
                    if alterations < 127:
                        sharps = alterations
                    else:
                        flats = 256 - alterations

                    k = Key(sharps, flats, minor)
                    if not t and global_options.key:
                        # At t == 0, a set --key overrides us
                        k = global_options.key
                    music.append((t, k))

                    # ugh, must set key while parsing
                    # because Note init uses key
                    # Better do Note.calc () at dump time?
                    global_options.key = k

                elif (e[1][1] == midi.LYRIC
                      or (global_options.text_lyrics
                          and e[1][1] == midi.TEXT_EVENT)):
                    self.lyrics_p_ = True
                    if last_lyric:
                        last_lyric.clocks = t - last_time
                        music.append((last_time, last_lyric))
                    last_time = t
                    last_lyric = Text(midi.LYRIC, e[1][2])

                elif (e[1][1] >= midi.SEQUENCE_NUMBER
                      and e[1][1] <= midi.CUE_POINT):
                    text = Text(e[1][1], e[1][2])
                    text.track = self
                    music.append((t, text))
                    if text.type == midi.SEQUENCE_TRACK_NAME:
                        self.name = text.text
                else:
                    if global_options.verbose:
                        sys.stderr.write("SKIP: %s\n" % repr(e))
            else:
                if global_options.verbose:
                    sys.stderr.write("SKIP: %s\n" % repr(e))

        if last_lyric:
            # last_lyric.clocks = t - last_time
            # hmm
            last_lyric.clocks = clocks_per_4
            music.append((last_time, last_lyric))
            last_lyric = 0

        i = 0
        while len(notes):
            if i < len(music) and notes[0][0] >= music[i][0]:
                i = i + 1
            else:
                music.insert(i, notes[0])
                del notes[0]

        if end_of_track_time is not None:
            music.append((end_of_track_time, EndOfTrack()))

        return music


class Track (Channel):
    def __init__(self):
        Channel.__init__(self, None)
        self.name = None
        self.channels = {}
        self.lyrics_p_ = False

    def _add(self, event):
        self.events.append(event)

    def add(self, event, channel=None):
        if channel is None:
            self._add(event)
        else:
            self.channels[channel] = self.channels.get(
                channel, Channel(channel))
            self.channels[channel].add(event)

    def get_voices(self):
        return ([self.get_voice()]
                + [self.channels[k].get_voice()
                   for k in sorted(self.channels.keys())])


def create_track(events):
    track = Track()
    for e in events:
        data = list(e[1])
        if data[0] > 0x7f and data[0] < 0xf0:
            channel = data[0] & 0x0f
            e = (e[0], tuple([data[0] & 0xf0] + data[1:]))
            track.add(e, channel)
        else:
            track.add(e)
    return track


def quantise_clocks(clocks, quant):
    q = int(clocks / quant) * quant
    if q != clocks:
        for tquant in allowed_tuplet_clocks:
            if int(clocks / tquant) * tquant == clocks:
                return clocks
        if 2 * (clocks - q) > quant:
            q = q + quant
    return q


def end_note(pitches, notes, t, e):
    try:
        (lt, vel) = pitches[e]
        del pitches[e]

        i = len(notes) - 1
        while i > 0:
            if notes[i][0] > lt:
                i = i - 1
            else:
                break
        d = t - lt
        if duration_quant_clocks:
            d = quantise_clocks(d, duration_quant_clocks)
            if not d:
                d = duration_quant_clocks

        notes.insert(i + 1,
                     (lt, Note(d, e, vel)))

    except KeyError:
        pass


def unthread_notes(channel):
    threads = []
    while channel:
        thread = []
        end_busy_t = 0
        start_busy_t = 0
        todo = []
        for e in channel:
            t = e[0]
            if (e[1].__class__ == Note
                and ((t == start_busy_t
                      and e[1].clocks + t == end_busy_t)
                     or t >= end_busy_t)):
                thread.append(e)
                start_busy_t = t
                end_busy_t = t + e[1].clocks
            elif (e[1].__class__ == Time
                  or e[1].__class__ == Key
                  or e[1].__class__ == Text
                  or e[1].__class__ == Tempo
                  or e[1].__class__ == EndOfTrack):
                thread.append(e)
            else:
                todo.append(e)
        threads.append(thread)
        channel = todo

    return threads


def dump_skip(skip, clocks):
    global reference_note
    saved_duration = reference_note.duration
    result = skip + Duration(clocks).dump() + ' '
    # "\skip D" does not change the reference duration like "sD",
    # so we restore it after Duration.dump changes it.
    if skip[0] == '\\':
        reference_note.duration = saved_duration
    return result


def dump(d):
    return d.dump()


def dump_chord(ch):
    s = ''
    notes = []
    for i in ch:
        if i.__class__ == Note:
            notes.append(i)
        else:
            s = s + i.dump()
    if len(notes) == 1:
        s = s + dump(notes[0])
    elif len(notes) > 1:
        global reference_note
        reference_dur = reference_note.duration
        s = s + '<'
        s = s + notes[0].dump(dump_dur=False)
        r = reference_note
        for i in notes[1:]:
            s = s + i.dump(dump_dur=False)
        s = s + '>'
        if (r.duration.compare(reference_dur)
                or global_options.explicit_durations):
            s = s + r.duration.dump()
        s = s + ' '
        reference_note = r
    return s


def dump_bar_line(last_bar_t, t, bar_count):
    s = ''
    bar_t = time.bar_clocks()
    if t - last_bar_t >= bar_t:
        bar_count = bar_count + (t - last_bar_t) / bar_t

        if t - last_bar_t == bar_t:
            s = '\n  | %% %(bar_count)d\n  ' % locals()
            last_bar_t = t
        else:
            # urg, this will barf at meter changes
            last_bar_t = last_bar_t + (t - last_bar_t) / bar_t * bar_t

    return (s, last_bar_t, bar_count)


def dump_voice(thread, skip):
    global reference_note, time
    ref = Note(0, 4*12, 0)
    if not reference_note:
        reference_note = ref
    else:
        ref.duration = reference_note.duration
        reference_note = ref
    last_e = None
    chs = []
    ch = []

    for e in thread:
        if last_e and last_e[0] == e[0]:
            ch.append(e[1])
        else:
            if ch:
                chs.append((last_e[0], ch))

            ch = [e[1]]

        last_e = e

    if ch:
        chs.append((last_e[0], ch))
    t = 0
    last_t = 0
    last_bar_t = 0
    bar_count = 1

    lines = ['']
    for ch in chs:
        t = ch[0]

        i = lines[-1].rfind('\n') + 1
        if len(lines[-1][i:]) > LINE_BELL:
            lines.append('')

        if t - last_t > 0:
            d = t - last_t
            if bar_max and t > time.bar_clocks() * bar_max:
                d = time.bar_clocks() * bar_max - last_t
            lines[-1] = lines[-1] + dump_skip(skip, d)
        elif t - last_t < 0:
            ly.error('BUG: time skew')

        (s, last_bar_t, bar_count) = dump_bar_line(last_bar_t,
                                                   t, bar_count)

        if bar_max and bar_count > bar_max:
            break

        lines[-1] = lines[-1] + s
        lines[-1] = lines[-1] + dump_chord(ch[1])

        clocks = 0
        for i in ch[1]:
            if i.clocks > clocks:
                clocks = i.clocks

        last_t = t + clocks

        (s, last_bar_t, bar_count) = dump_bar_line(last_bar_t,
                                                   last_t, bar_count)
        lines[-1] = lines[-1] + s

    return '\n  '.join(lines) + '\n'


def number2ascii(i):
    s = ''
    i += 1
    while i > 0:
        m = (i - 1) % 26
        s = '%c' % (m + ord('A')) + s
        i = (i - m) // 26
    return s


def get_track_name(i):
    return 'track' + number2ascii(i)


def get_channel_name(i):
    return 'channel' + number2ascii(i)


def get_voice_name(i, zero_too_p=False):
    if i or zero_too_p:
        return 'voice' + number2ascii(i)
    return ''


def lst_append(lst, x):
    lst.append(x)
    return lst


def get_voice_layout(average_pitch):
    d = {}
    for i in range(len(average_pitch)):
        d[average_pitch[i]] = lst_append(d.get(average_pitch[i], []), i)
    s = list(reversed(sorted(average_pitch)))
    non_empty = len([x for x in s if x])
    names = ['One', 'Two']
    if non_empty > 2:
        names = ['One', 'Three', 'Four', 'Two']
    layout = ['' for x in range(len(average_pitch))]
    for i, n in zip(s, names):
        if i:
            v = d[i]
            if isinstance(v, list):
                d[i] = v[1:]
                v = v[0]
            layout[v] = n
    return layout


def dump_track(track, n):
    s = '\n'
    track_name = get_track_name(n)

    average_pitch = track_average_pitch(track)
    voices = len([x for x in average_pitch[1:] if x])
    clef = get_best_clef(average_pitch[0])

    c = 0
    vv = 0
    for channel in track:
        v = 0
        channel_name = get_channel_name(c)
        c += 1
        for voice in channel:
            voice_name = get_voice_name(v)
            voice_id = track_name + channel_name + voice_name
            item = voice_first_item(voice)

            if item and item.__class__ == Note:
                skip = 'r'
                if global_options.skip:
                    skip = 's'
                s += '%(voice_id)s = ' % locals()
                if not global_options.absolute_pitches:
                    s += '\\relative c '
            elif item and item.__class__ == Text:
                skip = '" "'
                s += '%(voice_id)s = \\lyricmode ' % locals()
            else:
                skip = '\\skip '
                s += '%(voice_id)s = ' % locals()
            s += '{\n'
            if not n and not vv and global_options.key:
                s += global_options.key.dump()
            if average_pitch[vv+1] and voices > 1:
                vl = get_voice_layout(average_pitch[1:])[vv]
                if vl:
                    s += '  \\voice' + vl + '\n'
                else:
                    if not global_options.quiet:
                        ly.warning(
                            _('found more than 5 voices on a staff, expect bad output'))
            s += '  ' + dump_voice(voice, skip)
            s += '}\n\n'
            v += 1
            vv += 1

    s += '%(track_name)s = <<\n' % locals()

    if clef.type != 2:
        s += clef.dump() + '\n'

    c = 0
    vv = 0
    for channel in track:
        v = 0
        channel_name = get_channel_name(c)
        c += 1
        for voice in channel:
            voice_context_name = get_voice_name(vv, zero_too_p=True)
            voice_name = get_voice_name(v)
            v += 1
            vv += 1
            voice_id = track_name + channel_name + voice_name
            item = voice_first_item(voice)
            context = 'Voice'
            if item and item.__class__ == Text:
                context = 'Lyrics'
            s += '  \\context %(context)s = %(voice_context_name)s \\%(voice_id)s\n' % locals()
    s += '>>\n\n'
    return s


def voice_first_item(voice):
    for event in voice:
        if (event[1].__class__ == Note
            or (event[1].__class__ == Text
                and event[1].type == midi.LYRIC)):
            return event[1]
    return None


def channel_first_item(channel):
    for voice in channel:
        first = voice_first_item(voice)
        if first:
            return first
    return None


def track_first_item(track):
    for channel in track:
        first = channel_first_item(channel)
        if first:
            return first
    return None


def track_average_pitch(track):
    i = 0
    p = [0]
    v = 1
    for channel in track:
        for voice in channel:
            c = 0
            p.append(0)
            for event in voice:
                if event[1].__class__ == Note:
                    i += 1
                    c += 1
                    p[v] += event[1].pitch
            if c:
                p[0] += p[v]
                p[v] = p[v] / c
            v += 1
    if i:
        p[0] = p[0] / i
    return p


def get_best_clef(average_pitch):
    if average_pitch:
        if average_pitch <= 3*12:
            return Clef(0)
        elif average_pitch <= 5*12:
            return Clef(1)
        elif average_pitch >= 7*12:
            return Clef(3)
    return Clef(2)


class Staff:
    def __init__(self, track):
        self.voices = track.get_voices()

    def dump(self, i):
        return dump_track(self.voices, i)


def convert_midi(in_file, out_file):
    global midi
    import midi

    global clocks_per_1, clocks_per_4, key
    global start_quant_clocks
    global duration_quant_clocks
    global allowed_tuplet_clocks
    global time

    full_content = open(in_file, 'rb').read()
    clocks_max = bar_max * clocks_per_1 * 2
    midi_dump = midi.parse(full_content, clocks_max)

    clocks_per_1 = midi_dump[0][1]
    clocks_per_4 = clocks_per_1 / 4
    time = Time(4, 4)

    if global_options.start_quant:
        start_quant_clocks = clocks_per_1 / global_options.start_quant

    if global_options.duration_quant:
        duration_quant_clocks = clocks_per_1 / global_options.duration_quant

    allowed_tuplet_clocks = []
    for (dur, num, den) in global_options.allowed_tuplets:
        allowed_tuplet_clocks.append(clocks_per_1 / dur * num / den)

    if global_options.verbose:
        print('allowed tuplet clocks:', allowed_tuplet_clocks)

    tracks = [create_track(t) for t in midi_dump[1]]
    # urg, parse all global track events, such as Key first
    # this fixes key in different voice/staff problem
    for t in tracks:
        t.music = t.parse()
    prev = None
    staves = []
    for t in tracks:
        voices = t.get_voices()
        if ((t.name and prev and prev.name)
                and t.name.split(':')[0] == prev.name.split(':')[0]):
            # staves[-1].voices += voices
            # all global track events first
            staves[-1].voices = ([staves[-1].voices[0]]
                                 + [voices[0]]
                                 + staves[-1].voices[1:]
                                 + voices[1:])
        else:
            staves.append(Staff(t))
        prev = t

    tag = '%% Lily was here -- automatically converted by %s from %s' % (
        ly.program_name, in_file)

    s = tag
    s += r'''
\version "2.14.0"
'''

    s += r'''
\layout {
  \context {
    \Voice
    \remove Note_heads_engraver
    \consists Completion_heads_engraver
    \remove Rest_engraver
    \consists Completion_rest_engraver
  }
}
'''

    for i in global_options.include_header:
        s += '\n%% included from %(i)s\n' % locals()
        s += open(i, encoding='utf-8').read()
        if s[-1] != '\n':
            s += '\n'
        s += '% end\n'

    for i, t in enumerate(staves):
        s += t.dump(i)

    s += '\n\\score {\n  <<\n'

    control_track = False
    i = 0
    output_track_count = 0
    for i, staff in enumerate(staves):
        track_name = get_track_name(i)
        item = track_first_item(staff.voices)
        staff_name = track_name
        context = None
        if not i and not item and len(staves) > 1:
            control_track = track_name
            continue
        elif (item and item.__class__ == Note):
            context = 'Staff'
            if control_track:
                s += '    \\context %(context)s=%(staff_name)s \\%(control_track)s\n' % locals()
        elif item and item.__class__ == Text:
            context = 'Lyrics'
        if context:
            output_track_count += 1
            s += '    \\context %(context)s=%(staff_name)s \\%(track_name)s\n' % locals()

        # If we found a control track but no other tracks with which
        # to combine it, create a Staff for the control track alone.
        if (output_track_count == 0) and control_track:
            s += '    \\context Staff \\%(control_track)s\n' % locals()

    s = s + r'''  >>
  \layout {}
  \midi {}
}
'''

    if not global_options.quiet:
        ly.progress(_("%s output to `%s'...") % ('LY', out_file))

    if out_file == '-':
        handle = sys.stdout
    else:
        handle = open(out_file, 'w', encoding='utf-8')

    handle.write(s)
    handle.close()


def get_option_parser():
    p = ly.get_option_parser(usage=_("%s [OPTION]... FILE") % 'midi2ly',
                             description=_(
                                 "Convert %s to LilyPond input.\n") % 'MIDI',
                             add_help_option=False)

    p.add_option('-a', '--absolute-pitches',
                 action='store_true',
                 help=_('print absolute pitches'))
    p.add_option('-d', '--duration-quant',
                 metavar=_('DUR'),
                 help=_('quantise note durations on DUR'))
    p.add_option('-D', '--debug',
                 action='store_true',
                 help=_('debug printing'))
    p.add_option('-e', '--explicit-durations',
                 action='store_true',
                 help=_('print explicit durations'))
    p.add_option('-h', '--help',
                 action='help',
                 help=_('show this help and exit'))
    p.add_option('-i', '--include-header',
                 help=_('prepend FILE to output'),
                 action='append',
                 default=[],
                 metavar=_('FILE'))
    p.add_option('-k', '--key', help=_('set key: ALT=+sharps|-flats; MINOR=1'),
                 metavar=_('ALT[:MINOR]'),
                 default=None),
    p.add_option('-o', '--output', help=_('write output to FILE'),
                 metavar=_('FILE'),
                 action='store')
    p.add_option('-p', '--preview', help=_('preview of first 4 bars'),
                 action='store_true')
    p.add_option('-q', '--quiet',
                 action="store_true",
                 help=_("suppress progress messages and warnings about excess voices"))
    p.add_option('-s', '--start-quant', help=_('quantise note starts on DUR'),
                 metavar=_('DUR'))
    p.add_option('-S', '--skip',
                 action="store_true",
                 help=_("use s instead of r for rests"))
    p.add_option('-t', '--allow-tuplet',
                 metavar=_('DUR*NUM/DEN'),
                 action='append',
                 dest='allowed_tuplets',
                 help=_('allow tuplet durations DUR*NUM/DEN'),
                 default=[])
    p.add_option('-V', '--verbose', help=_('be verbose'),
                 action='store_true')
    p.version = 'midi2ly (LilyPond) @TOPLEVEL_VERSION@'
    p.add_option('--version',
                 action='version',
                 help=_('show version number and exit'))
    p.add_option('-w', '--warranty', help=_('show warranty and copyright'),
                 action='store_true',)
    p.add_option('-x', '--text-lyrics', help=_('treat every text as a lyric'),
                 action='store_true')

    p.add_option_group(_('Examples'),
                       description=r'''
  $ midi2ly --key=-2:1 --duration-quant=32 --allow-tuplet=4*2/3 --allow-tuplet=2*4/3 foo.midi
''')
    p.add_option_group('',
                       description=(
                           _('Report bugs via %s')
                           % 'bug-lilypond@gnu.org') + '\n')
    return p


def do_options():
    opt_parser = get_option_parser()
    (options, args) = opt_parser.parse_args()

    if options.warranty:
        warranty()
        sys.exit(0)

    if not args or args[0] == '-':
        opt_parser.print_help()
        sys.stderr.write('\n%s: %s %s\n' % (ly.program_name, _('error: '),
                                            _('no files specified on command line.')))
        sys.exit(2)

    if options.duration_quant:
        options.duration_quant = int(options.duration_quant)

    if options.key:
        (alterations, minor) = list(
            map(int, (options.key + ':0').split(':')))[0:2]
        sharps = 0
        flats = 0
        if alterations >= 0:
            sharps = alterations
        else:
            flats = - alterations
        options.key = Key(sharps, flats, minor)

    if options.start_quant:
        options.start_quant = int(options.start_quant)

    global bar_max
    if options.preview:
        bar_max = 4

    options.allowed_tuplets = [list(map(int, a.replace('/', '*').split('*')))
                               for a in options.allowed_tuplets]

    if options.verbose:
        sys.stderr.write('Allowed tuplets: %s\n' %
                         repr(options.allowed_tuplets))

    global global_options
    global_options = options

    return args


def main():
    files = do_options()

    exts = ['.midi', '.mid', '.MID']
    for f in files:
        g = f
        for e in exts:
            g = strip_extension(g, e)
        if not os.path.exists(f):
            for e in exts:
                n = g + e
                if os.path.exists(n):
                    f = n
                    break

        if not global_options.output:
            outdir = '.'
            outbase = os.path.basename(g)
            o = outbase + '-midi.ly'
        elif (global_options.output[-1] == os.sep
              or os.path.isdir(global_options.output)):
            outdir = global_options.output
            outbase = os.path.basename(g)
            o = os.path.join(outdir, outbase + '-midi.ly')
        else:
            o = global_options.output
            (outdir, outbase) = os.path.split(o)

        if outdir and outdir != '.' and not os.path.exists(outdir):
            os.mkdir(outdir, 0o777)

        convert_midi(f, o)


if __name__ == '__main__':
    main()
