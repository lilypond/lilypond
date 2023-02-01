#!@TARGET_PYTHON@

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

# info mostly taken from looking at files. See also
# https://www.gnu.org/software/lilypond/src/Developers/Details/etfformat.html

# This supports
#
#  * notes
#  * rests
#  * ties
#  * slurs
#  * lyrics
#  * articulation
#  * grace notes
#  * tuplets
#

# todo:
#  * slur/stem directions
#  * voices (2nd half of frame?)
#  * more intelligent lyrics
#  * beams (better use autobeam?)
#  * more robust: try entertainer.etf (freenote)
#  * dynamics
#  * empty measures (eg. twopt03.etf from freenote)
#


import __main__
import getopt
import gettext
import os
import re
import sys

authors = ('Jan Nieuwenhuizen <janneke@gnu.org>',
           'Han-Wen Nienhuys <hanwen@xs4all.nl>')

version = '@TOPLEVEL_VERSION@'
if version == '@' + 'TOPLEVEL_VERSION' + '@':
    version = '(unknown version)'           # uGUHGUHGHGUGH

"""
@relocate-preamble@
"""

################################################################
# Load translation and install _() into Python's builtins namespace.
gettext.install('lilypond', '@localedir@')

import lilylib as ly

finale_clefs = ['treble', 'alto', 'tenor', 'bass',
                'percussion', 'treble_8', 'bass_8', 'baritone']


def lily_clef(fin):
    try:
        return finale_clefs[fin]
    except IndexError:
        sys.stderr.write('\nHuh? Found clef number %d\n' % fin)

    return 'treble'


def gulp_file(f):
    return open(f, encoding='utf-8').read()


# notename 0 == central C
distances = [0, 2, 4, 5, 7, 9, 11, 12]


def semitones(name, acc):
    return (name / 7) * 12 + distances[name % 7] + acc

# represent pitches as (notename, alteration), relative to C-major scale


def transpose(orig, delta):
    (oname, oacc) = orig
    (dname, dacc) = delta

    old_pitch = semitones(oname, oacc)
    delta_pitch = semitones(dname, dacc)
    nname = (oname + dname)
    nacc = oacc
    new_pitch = semitones(nname, nacc)

    nacc = nacc - (new_pitch - old_pitch - delta_pitch)

    return (nname, nacc)


def interpret_finale_key_sig(finale_id):
    """
find the transposition of C-major scale that belongs here.

we are not going to insert the correct major/minor, we only want to
have the correct number of accidentals
"""

    p = (0, 0)

    bank_number = finale_id >> 8
    accidental_bits = finale_id & 0xff

    if 0 <= accidental_bits < 7:
        while accidental_bits > 0:
            p = transpose(p, (4, 0))  # a fifth up
            accidental_bits = accidental_bits - 1
    elif 248 < accidental_bits <= 255:
        while accidental_bits < 256:
            p = transpose(p, (3, 0))
            accidental_bits = accidental_bits + 1

    if bank_number == 1:
        # minor scale
        p = transpose(p, (5, 0))
    p = (p[0] % 7, p[1])

    return KeySignature(p, bank_number)

# should cache this.


def find_scale(keysig):
    cscale = [(x, 0) for x in range(0, 7)]
#        print "cscale: ", cscale
    ascale = [(x, 0) for x in range(-2, 5)]
#        print "ascale: ", ascale
    transposition = keysig.pitch
    if keysig.sig_type == 1:
        transposition = transpose(transposition, (2, -1))
        transposition = (transposition[0] % 7, transposition[1])
        trscale = list(map(lambda x, k=transposition: transpose(x, k), ascale))
    else:
        trscale = list(map(lambda x, k=transposition: transpose(x, k), cscale))
#        print "trscale: ", trscale
    return trscale


def EDU_to_duration(edu):
    log = 1
    d = 4096
    while d > edu:
        d = d >> 1
        log = log << 1

    edu = edu - d
    dots = 0
    if edu == d / 2:
        dots = 1
    elif edu == d*3/4:
        dots = 2
    return (log, dots)


def rational_to_lily_skip(rat):
    (n, d) = rat

    basedur = 1
    while d and d % 2 == 0:
        basedur = basedur << 1
        d = d >> 1

    str = 's%d' % basedur
    if n != 1:
        str = str + '*%d' % n
    if d != 1:
        str = str + '/%d' % d

    return str


def gcd(a, b):
    if b == 0:
        return a
    c = a
    while c:
        c = a % b
        a = b
        b = c
    return a


def rat_simplify(r):
    (n, d) = r
    if d < 0:
        d = -d
        n = -n
    if n == 0:
        return (0, 1)
    else:
        g = gcd(n, d)
        return (n/g, d/g)


def rat_multiply(a, b):
    (x, y) = a
    (p, q) = b

    return rat_simplify((x*p, y*q))


def rat_add(a, b):
    (x, y) = a
    (p, q) = b

    return rat_simplify((x*q + p*y, y*q))


def rat_neg(a):
    (p, q) = a
    return (-p, q)


def rat_subtract(a, b):
    return rat_add(a, rat_neg(b))


def lily_notename(tuple2):
    (n, a) = tuple2
    nn = chr((n + 2) % 7 + ord('a'))

    return nn + {-2: 'eses', -1: 'es', 0: '', 1: 'is', 2: 'isis'}[a]


class Tuplet:
    def __init__(self, number):
        self.start_note = number
        self.finale = []

    def append_finale(self, fin):
        self.finale.append(fin)

    def factor(self):
        n = self.finale[0][2]*self.finale[0][3]
        d = self.finale[0][0]*self.finale[0][1]
        return rat_simplify((n, d))

    def dump_start(self):
        return '\\times %d/%d { ' % self.factor()

    def dump_end(self):
        return ' }'

    def calculate(self, chords):
        edu_left = self.finale[0][0] * self.finale[0][1]

        startch = chords[self.start_note]
        c = startch
        while c and edu_left:
            c.tuplet = self
            if c == startch:
                c.chord_prefix = self.dump_start() + c.chord_prefix

            if not c.grace:
                edu_left = edu_left - c.EDU_duration()
            if edu_left == 0:
                c.chord_suffix = c.chord_suffix + self.dump_end()
            c = c.__next__

        if edu_left:
            sys.stderr.write(
                "\nHuh? Tuplet starting at entry %d was too short." % self.start_note)


class Slur:
    def __init__(self, number, params):
        self.number = number
        self.finale = params

    def append_entry(self, finale_e):
        self.finale.append(finale_e)

    def calculate(self, chords):
        startnote = self.finale[5]
        endnote = self.finale[3*6 + 2]
        try:
            cs = chords[startnote]
            ce = chords[endnote]

            if not cs or not ce:
                raise IndexError

            cs.note_suffix = '-(' + cs.note_suffix
            ce.note_suffix = ce.note_suffix + '-)'

        except IndexError:
            sys.stderr.write("""\nHuh? Slur no %d between (%d,%d), with %d notes""" % (
                self.number,  startnote, endnote, len(chords)))


class Global_measure:
    def __init__(self, number):
        self.timesig = ''
        self.number = number
        self.key_signature = None
        self.scale = None
        self.force_break = 0

        self.repeats = []
        self.finale = []

    def __str__(self):
        return repr(self.finale)

    def set_timesig(self, finale):
        (beats, fdur) = finale
        (log, dots) = EDU_to_duration(fdur)

        if dots == 1:
            beats = beats * 3
            log = log * 2
            dots = 0

        if dots != 0:
            sys.stderr.write(
                "\nHuh? Beat duration has  dots? (EDU Duration = %d)" % fdur)
        self.timesig = (beats, log)

    def length(self):
        return self.timesig

    def set_key_sig(self, finale):
        k = interpret_finale_key_sig(finale)
        self.key_signature = k
        self.scale = find_scale(k)

    def set_flags(self, flag1, flag2):

        # flag1 isn't all that interesting.
        if flag2 & 0x8000:
            self.force_break = 1

        if flag2 & 0x0008:
            self.repeats.append('start')
        if flag2 & 0x0004:
            self.repeats.append('stop')

        if flag2 & 0x0002:
            if flag2 & 0x0004:
                self.repeats.append('bracket')


articulation_dict = {
    94: '^',
    109: '\\prall',
    84: '\\turn',
    62: '\\mordent',
    85: '\\fermata',
    46: '.',
    #        3: '>',
    #        18: '\arpeggio' ,
}


class Articulation_def:
    def __init__(self, n, a, b):
        self.finale_glyph = a & 0xff
        self.number = n

    def dump(self):
        try:
            return articulation_dict[self.finale_glyph]
        except KeyError:
            sys.stderr.write("\nUnknown articulation no. %d" %
                             self.finale_glyph)
            sys.stderr.write(
                "\nPlease add an entry to articulation_dict in the Python source")
            return None


class Articulation:
    def __init__(self, a, b, finale):
        self.definition = finale[0]
        self.notenumber = b

    def calculate(self, chords, defs):
        c = chords[self.notenumber]

        adef = defs[self.definition]
        lystr = adef.dump()
        if lystr is None:
            lystr = '"art"'
            sys.stderr.write("\nThis happened on note %d" % self.notenumber)

        c.note_suffix = '-' + lystr


class Syllable:
    def __init__(self, a, b, finale):
        self.chordnum = b
        self.syllable = finale[1]
        self.verse = finale[0]

    def calculate(self, chords, lyrics):
        self.chord = chords[self.chordnum]


class Verse:
    def __init__(self, number, body):
        self.body = body
        self.number = number
        self.split_syllables()

    def split_syllables(self):
        ss = re.split('(-| +)', self.body)

        sep = 0
        syls = [None]
        for s in ss:
            if sep:
                septor = re.sub(" +", "", s)
                septor = re.sub("-", " -- ", septor)
                syls[-1] = syls[-1] + septor
            else:
                syls.append(s)

            sep = not sep

        self.syllables = syls

    def dump(self):
        str = ''
        line = ''
        for s in self.syllables[1:]:
            line = line + ' ' + s
            if len(line) > 72:
                str = str + ' ' * 4 + line + '\n'
                line = ''

        str = """\nverse%s = \\lyricmode {\n %s }\n""" % (
            encodeint(self.number - 1), str)
        return str


class KeySignature:
    def __init__(self, pitch, sig_type=0):
        self.pitch = pitch
        self.sig_type = sig_type

    def signature_type(self):
        if self.sig_type == 1:
            return "\\minor"
        else:
            # really only for 0, but we only know about 0 and 1
            return "\\major"

    def equal(self, other):
        if other and other.pitch == self.pitch and other.sig_type == self.sig_type:
            return 1
        else:
            return 0


class Measure:
    def __init__(self, no):
        self.number = no
        self.frames = [0] * 4
        self.flags = 0
        self.clef = 0
        self.finale = []
        self.global_measure = None
        self.staff = None
        self.valid = 1

    def valid(self):
        return self.valid

    def calculate(self):
        fs = []

        if len(self.finale) < 2:
            fs = self.finale[0]

            self.clef = fs[1]
            self.frames = [fs[0]]
        else:
            fs = self.finale
            self.clef = fs[0]
            self.flags = fs[1]
            self.frames = fs[2:]


class Frame:
    def __init__(self, finale):
        self.measure = None
        self.finale = finale
        (number, start, end) = finale
        self.number = number
        self.start = start
        self.end = end
        self.chords = []

    def set_measure(self, m):
        self.measure = m

    def calculate(self):

        # do grace notes.
        lastch = None
        in_grace = 0
        for c in self.chords:
            if c.grace and (lastch is None or (not lastch.grace)):
                c.chord_prefix = r'\grace {' + c.chord_prefix
                in_grace = 1
            elif not c.grace and lastch and lastch.grace:
                lastch.chord_suffix = lastch.chord_suffix + ' } '
                in_grace = 0

            lastch = c

        if lastch and in_grace:
            lastch.chord_suffix += '}'

    def dump(self):
        str = '%% FR(%d)\n' % self.number
        left = self.measure.global_measure.length()

        ln = ''
        for c in self.chords:
            add = c.ly_string() + ' '
            if len(ln) + len(add) > 72:
                str = str + ln + '\n'
                ln = ''
            ln = ln + add
            left = rat_subtract(left, c.length())

        str = str + ln

        if left[0] < 0:
            sys.stderr.write("""\nHuh? Going backwards in frame no %d, start/end (%d,%d)""" %
                             (self.number, self.start, self.end))
            left = (0, 1)
        if left[0]:
            str = str + rational_to_lily_skip(left)

        str = str + '  |\n'
        return str


def encodeint(i):
    return chr(i + ord('A'))


class Staff:
    def __init__(self, number):
        self.number = number
        self.measures = []

    def get_measure(self, no):
        fill_list_to(self.measures, no)

        if self.measures[no] is None:
            m = Measure(no)
            self.measures[no] = m
            m.staff = self

        return self.measures[no]

    def staffid(self):
        return 'staff' + encodeint(self.number - 1)

    def layerid(self, l):
        return self.staffid() + 'layer%s' % chr(l - 1 + ord('A'))

    def dump_time_key_sigs(self):
        k = ''
        last_key = None
        last_time = None
        last_clef = None
        gap = (0, 1)
        for m in self.measures[1:]:
            if not m or not m.valid:
                continue  # ugh.

            g = m.global_measure
            e = ''

            if g:
                if g.key_signature and not g.key_signature.equal(last_key):
                    pitch = g.key_signature.pitch
                    e = e + "\\key %s %s " % (lily_notename(pitch),
                                              g.key_signature.signature_type())

                    last_key = g.key_signature
                if last_time != g.timesig:
                    e = e + "\\time %d/%d " % g.timesig
                    last_time = g.timesig

                if 'start' in g.repeats:
                    e = e + ' \\bar ".|:" '

                # we don't attempt voltas since they fail easily.
                if 0:  # and g.repeat_bar == '|:' or g.repeat_bar == ':|:' or g.bracket:
                    strs = []
                    if g.repeat_bar == '|:' or g.repeat_bar == ':|:' or g.bracket == 'end':
                        strs.append('#f')

                    if g.bracket == 'start':
                        strs.append('"0."')

                    str = ' '.join(['(volta %s)' % x for x in strs])

                    e = e + ' \\set Score.repeatCommands =  #\'(%s) ' % str

                if g.force_break:
                    e = e + ' \\break '

            if last_clef != m.clef:
                e = e + '\\clef "%s"' % lily_clef(m.clef)
                last_clef = m.clef
            if e:
                if gap != (0, 1):
                    k = k + ' ' + rational_to_lily_skip(gap) + '\n'
                gap = (0, 1)
                k = k + e

            if g:
                gap = rat_add(gap, g.length())
                if 'stop' in g.repeats:
                    k = k + ' \\bar ":|." '

        k = '%sglobal = { %s }\n\n ' % (self.staffid(), k)
        return k

    def dump(self):
        str = ''

        layerids = []
        for x in range(1, 5):  # 4 layers.
            laystr = ''
            last_frame = None
            first_frame = None
            gap = (0, 1)
            for m in self.measures[1:]:
                if not m or not m.valid:
                    sys.stderr.write(
                        "Skipping non-existant or invalid measure\n")
                    continue

                fr = None
                try:
                    fr = m.frames[x]
                except IndexError:
                    sys.stderr.write("Skipping nonexistent frame %d\n" % x)
                    laystr = laystr + \
                        "%% non existent frame %d (skipped)\n" % x
                if fr:
                    first_frame = fr
                    if gap != (0, 1):
                        laystr = laystr + \
                            '} %s {\n ' % rational_to_lily_skip(gap)
                        gap = (0, 1)
                    laystr = laystr + fr.dump()
                else:
                    if m.global_measure:
                        gap = rat_add(gap, m.global_measure.length())
                    else:
                        sys.stderr.write(
                            "No global measure for staff %d measure %d\n"
                            % (self.number, m.number))
            if first_frame:
                l = self.layerid(x)
                laystr = '%s = { {  %s } }\n\n' % (l, laystr)
                str = str + laystr
                layerids.append(l)

        str = str + self.dump_time_key_sigs()
        stafdef = '\\%sglobal' % self.staffid()
        for i in layerids:
            stafdef = stafdef + ' \\' + i

        str = str + '%s = \\context Staff = %s <<\n %s\n >>\n' % \
            (self.staffid(), self.staffid(), stafdef)
        return str


def ziplist(l):
    if len(l) < 2:
        return []
    else:
        return [(l[0], l[1])] + ziplist(l[2:])


class Chord:
    def __init__(self, number, contents):
        self.pitches = []
        self.frame = None
        self.finale = contents[:7]

        self.notelist = ziplist(contents[7:])
        self.duration = None
        self.next = None
        self.prev = None
        self.number = number
        self.note_prefix = ''
        self.note_suffix = ''
        self.chord_suffix = ''
        self.chord_prefix = ''
        self.tuplet = None
        self.grace = 0

    def measure(self):
        if not self.frame:
            return None
        return self.frame.measure

    def length(self):
        if self.grace:
            return (0, 1)

        l = (1, self.duration[0])

        d = 1 << self.duration[1]

        dotfact = rat_subtract((2, 1), (1, d))
        mylen = rat_multiply(dotfact, l)

        if self.tuplet:
            mylen = rat_multiply(mylen, self.tuplet.factor())
        return mylen

    def EDU_duration(self):
        return self.finale[2]

    def set_duration(self):
        self.duration = EDU_to_duration(self.EDU_duration())

    def calculate(self):
        self.find_realpitch()
        self.set_duration()

        flag = self.finale[4]
        if Chord.GRACE_MASK & flag:
            self.grace = 1

    def find_realpitch(self):

        meas = self.measure()
        tiestart = 0
        if not meas or not meas.global_measure:
            sys.stderr.write('note %d not in measure\n' % self.number)
        elif not meas.global_measure.scale:
            sys.stderr.write(
                'note %d: no scale in this measure.' % self.number)
        else:

            for p in self.notelist:
                (pitch, flag) = p

                nib1 = pitch & 0x0f

                if nib1 > 8:
                    nib1 = -(nib1 - 8)
                rest = pitch / 16

                scale = meas.global_measure.scale
                (sn, sa) = scale[rest % 7]
                sn = sn + (rest - (rest % 7)) + 7
                acc = sa + nib1
                self.pitches.append((sn, acc))
                tiestart = tiestart or (flag & Chord.TIE_START_MASK)
        if tiestart:
            self.chord_suffix = self.chord_suffix + ' ~ '

    REST_MASK = 0x40000000
    TIE_START_MASK = 0x40000000
    GRACE_MASK = 0x00800000

    def ly_string(self):
        s = ''

        rest = ''

        if not (self.finale[4] & Chord.REST_MASK):
            rest = 'r'

        for p in self.pitches:
            (n, a) = p
            o = n / 7
            n = n % 7

            nn = lily_notename((n, a))

            if o < 0:
                nn = nn + (',' * -o)
            elif o > 0:
                nn = nn + ('\'' * o)

            if s:
                s = s + ' '

            if rest:
                nn = rest

            s = s + nn

        if not self.pitches:
            s = 'r'
        if len(self.pitches) > 1:
            s = '<%s>' % s

        s = s + '%d%s' % (self.duration[0], '.' * self.duration[1])
        s = self.note_prefix + s + self.note_suffix

        s = self.chord_prefix + s + self.chord_suffix

        return s


def fill_list_to(list, no):
    """
Add None to LIST until it contains entry number NO.
    """
    while len(list) <= no:
        list.extend([None] * (no - len(list) + 1))
    return list


def read_finale_value(str):
    """
Pry off one value from STR. The value may be $hex, decimal, or "string".
Return: (value, rest-of-STR)
    """
    while str and str[0] in ' \t\n':
        str = str[1:]

    if not str:
        return (None, str)

    if str[0] == '$':
        str = str[1:]

        hex = ''
        while str and str[0] in '0123456789ABCDEF':
            hex = hex + str[0]
            str = str[1:]

        return (int(hex, 16), str)
    elif str[0] == '"':
        str = str[1:]
        s = ''
        while str and str[0] != '"':
            s = s + str[0]
            str = str[1:]

        return (s, str)
    elif str[0] in '-0123456789':
        dec = ''
        while str and str[0] in '-0123456789':
            dec = dec + str[0]
            str = str[1:]

        return (int(dec), str)
    else:
        sys.stderr.write("cannot convert `%s'\n" % str)
        return (None, str)


def parse_etf_file(fn, tag_dict):
    """ Read FN, putting ETF info into
    a giant dictionary.  The keys of TAG_DICT indicate which tags
    to put into the dict.
    """

    sys.stderr.write('parsing ... ')
    f = open(fn, encoding='utf-8')

    gulp = re.sub('[\n\r]+', '\n',  f.read())
    ls = gulp.split('\n^')

    etf_file_dict = {}
    for k in tag_dict:
        etf_file_dict[k] = {}

    last_tag = None
    last_numbers = None

    for l in ls:
        m = re.match(r'^([a-zA-Z0-9&]+)\(([^)]+)\)', l)
        if m and m.group(1) in tag_dict:
            tag = m.group(1)

            indices = tuple([int(s) for s in m.group(2).split(',')])
            content = l[m.end(2)+1:]

            tdict = etf_file_dict[tag]
            if indices not in tdict:
                tdict[indices] = []

            parsed = []

            if tag == 'verse' or tag == 'block':
                m2 = re.match(r'(.*)\^end', content)
                if m2:
                    parsed = [m2.group(1)]
            else:
                while content:
                    (v, content) = read_finale_value(content)
                    if v is not None:
                        parsed.append(v)

            tdict[indices].extend(parsed)

            last_indices = indices
            last_tag = tag

            continue

# let's not do this: this really confuses when eE happens to be before  a ^text.
#                if last_tag and last_indices:
#                        etf_file_dict[last_tag][last_indices].append (l)

    sys.stderr.write('\n')
    return etf_file_dict


class Etf_file:
    def __init__(self, name):
        self.measures = [None]
        self.chords = [None]
        self.frames = [None]
        self.tuplets = [None]
        self.staffs = [None]
        self.slurs = [None]
        self.articulations = [None]
        self.syllables = [None]
        self.verses = [None]
        self.articulation_defs = [None]

        # do it
        self.parse(name)

    def get_global_measure(self, no):
        fill_list_to(self.measures, no)
        if self.measures[no] is None:
            self.measures[no] = Global_measure(no)

        return self.measures[no]

    def get_staff(self, staffno):
        fill_list_to(self.staffs, staffno)
        if self.staffs[staffno] is None:
            self.staffs[staffno] = Staff(staffno)

        return self.staffs[staffno]

    # staff-spec
    def try_IS(self, indices, contents):
        pass

    def try_BC(self, indices, contents):
        bn = indices[0]
        where = contents[0] / 1024.0

    def try_TP(self,  indices, contents):
        (nil, num) = indices

        if self.tuplets[-1] is None or num != self.tuplets[-1].start_note:
            self.tuplets.append(Tuplet(num))

        self.tuplets[-1].append_finale(contents)

    def try_IM(self, indices, contents):
        (a, b) = indices
        fin = contents
        self.articulations.append(Articulation(a, b, fin))

    def try_verse(self, indices, contents):
        a = indices[0]
        body = contents[0]

        body = re.sub(r"""\^[a-z]+\([^)]+\)""", "", body)
        body = re.sub(r"\^[a-z]+", "", body)
        self.verses.append(Verse(a, body))

    def try_ve(self, indices, contents):
        (a, b) = indices
        self.syllables.append(Syllable(a, b, contents))

    def try_eE(self, indices, contents):
        no = indices[0]
        (prev, next, dur, pos, entryflag, extended, follow) = contents[:7]

        fill_list_to(self.chords, no)
        self.chords[no] = Chord(no, contents)

    def try_Sx(self, indices, contents):
        slurno = indices[0]
        fill_list_to(self.slurs, slurno)
        self.slurs[slurno] = Slur(slurno, contents)

    def try_IX(self, indices, contents):
        n = indices[0]
        a = contents[0]
        b = contents[1]

        ix = None
        try:
            ix = self.articulation_defs[n]
        except IndexError:
            ix = Articulation_def(n, a, b)
            self.articulation_defs.append(Articulation_def(n, a, b))

    def try_GF(self, indices, contents):
        (staffno, measno) = indices

        st = self.get_staff(staffno)
        meas = st.get_measure(measno)
        meas.finale = contents

    def try_FR(self, indices, contents):
        frameno = indices[0]

        startnote = contents[0]
        endnote = contents[1]

        fill_list_to(self.frames, frameno)

        self.frames[frameno] = Frame((frameno, startnote, endnote))

    def try_MS(self, indices, contents):
        measno = indices[0]
        keynum = contents[1]
        meas = self. get_global_measure(measno)

        meas.set_key_sig(keynum)

        beats = contents[2]
        beatlen = contents[3]
        meas.set_timesig((beats, beatlen))

        meas_flag1 = contents[4]
        meas_flag2 = contents[5]

        meas.set_flags(meas_flag1, meas_flag2)

    routine_dict = {
        'MS': try_MS,
        'FR': try_FR,
        'GF': try_GF,
        'IX': try_IX,
        'Sx': try_Sx,
        'eE': try_eE,
        'verse': try_verse,
        've': try_ve,
        'IM': try_IM,
        'TP': try_TP,
        'BC': try_BC,
        'IS': try_IS,
    }

    def parse(self, etf_dict):
        sys.stderr.write('reconstructing ...')
        sys.stderr.flush()

        for (tag, routine) in list(Etf_file.routine_dict.items()):
            ks = list(etf_dict[tag].keys())
            ks.sort()
            for k in ks:
                routine(self, k, etf_dict[tag][k])

        sys.stderr.write('processing ...')
        sys.stderr.flush()

        self.unthread_entries()

        for st in self.staffs[1:]:
            if not st:
                continue
            mno = 1
            for m in st.measures[1:]:
                if not m:
                    continue

                m.calculate()
                try:
                    m.global_measure = self.measures[mno]
                except IndexError:
                    sys.stderr.write("Non-existent global measure %d" % mno)
                    continue

                frame_obj_list = [None]
                for frno in m.frames:
                    try:
                        fr = self.frames[frno]
                        frame_obj_list.append(fr)
                    except IndexError:
                        sys.stderr.write("\nNon-existent frame %d" % frno)

                m.frames = frame_obj_list
                for fr in frame_obj_list[1:]:
                    if not fr:
                        continue

                    fr.set_measure(m)

                    fr.chords = self.get_thread(fr.start, fr.end)
                    for c in fr.chords:
                        c.frame = fr
                mno = mno + 1

        for c in self.chords[1:]:
            if c:
                c.calculate()

        for f in self.frames[1:]:
            if f:
                f.calculate()

        for t in self.tuplets[1:]:
            t.calculate(self.chords)

        for s in self.slurs[1:]:
            if s:
                s.calculate(self.chords)

        for s in self.articulations[1:]:
            s.calculate(self.chords, self.articulation_defs)

    def get_thread(self, startno, endno):

        thread = []

        c = None
        try:
            c = self.chords[startno]
        except IndexError:
            sys.stderr.write(
                "Huh? Frame has invalid bounds (%d,%d)\n" % (startno, endno))
            return []

        while c and c.number != endno:
            d = c  # hack to avoid problem with scripts/build/grand-replace.py
            thread.append(d)
            c = c.__next__

        if c:
            d = c  # hack to avoid problem with scripts/build/grand-replace.py
            thread.append(d)

        return thread

    def dump(self):
        str = ''
        staffs = []
        for s in self.staffs[1:]:
            if s:
                str = str + '\n\n' + s.dump()
                staffs.append('\\' + s.staffid())

        # should use \addlyrics ?

        for v in self.verses[1:]:
            str = str + v.dump()

        if len(self.verses) > 1:
            sys.stderr.write(
                "\nLyrics found; edit to use \\addlyrics to couple to a staff\n")

        if staffs:
            str += '\\version "2.3.25"\n'
            str = str + '<<\n  %s\n>> } ' % ' '.join(staffs)

        return str

    def __str__(self):
        return 'ETF FILE %s %s' % (self.measures,  self.entries)

    def unthread_entries(self):
        for e in self.chords[1:]:
            if not e:
                continue

            e.prev = self.chords[e.finale[0]]
            e.next = self.chords[e.finale[1]]


def identify():
    sys.stderr.write("%s from LilyPond %s\n" % (ly.program_name, version))


def warranty():
    identify()
    sys.stdout.write('''
%s

  %s

%s
%s
''' % (_('Copyright (c) %s by') % '2001--2023',
        '\n  '.join(authors),
        _('Distributed under terms of the GNU General Public License.'),
        _('It comes with NO WARRANTY.')))


def get_option_parser():
    p = ly.get_option_parser(usage=_("%s [OPTION]... ETF-FILE") % 'etf2ly',
                             description=_("""Enigma Transport Format is a format used by Coda Music Technology's
Finale product.  etf2ly converts a subset of ETF to a ready-to-use LilyPond file.
"""),
                             add_help_option=False)
    p.add_option("-h", "--help",
                 action="help",
                 help=_("show this help and exit"))
    p.version = "etf2ly (LilyPond) @TOPLEVEL_VERSION@"
    p.add_option("--version",
                 action="version",
                 help=_("show version number and exit"))
    p.add_option('-o', '--output', help=_("write output to FILE"),
                 metavar=_("FILE"),
                 action='store')
    p.add_option('-w', '--warranty', help=_("show warranty and copyright"),
                 action='store_true',
                 ),

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

    return (options, args)


(options, files) = do_options()
identify()

out_filename = options.output

e = None
for f in files:
    if f == '-':
        f = ''

    sys.stderr.write('Processing `%s\'\n' % f)

    dict = parse_etf_file(f, Etf_file.routine_dict)
    e = Etf_file(dict)
    if not out_filename:
        out_filename = os.path.basename(re.sub('(?i).etf$', '.ly', f))

    if out_filename == f:
        out_filename = os.path.basename(f + '.ly')

    sys.stderr.write('Writing `%s\'' % out_filename)
    ly = e.dump()

    fo = open(out_filename, 'w', encoding='utf-8')
    fo.write('%% lily was here -- automatically converted by etf2ly from %s\n' % f)
    fo.write(ly)
    fo.close()
