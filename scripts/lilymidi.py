#!@TARGET_PYTHON@

# Copyright (C) 2006--2015 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# This file is part of LilyPond, the GNU music typesetter.
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

import optparse
import os
import sys

"""
@relocate-preamble@
"""

def process_options (args):
    parser = optparse.OptionParser (version="@TOPLEVEL_VERSION@")
    parser.add_option ('', '--filter-tracks', metavar='REGEXP', action='store', type='string', dest='regexp',
                       help="display only tracks numbers, of those track names matching REGEXP")
    parser.add_option ('', '--prefix-tracks', metavar='PREFIX', action='store', type='string', dest='prefix',
                       help="prefix filtered track numbers with PREFIX")
    parser.add_option ('', '--dump', action='store_true', dest='dump',
                       help="just dump parsed contents of the MIDI file")
    parser.add_option ('', '--pretty', action='store_true', dest='pretty',
                       help="dump parsed contents of the MIDI file in human-readable form (implies --dump)")
    parser.usage = parser.usage + " FILE"
    options, args = parser.parse_args (args)
    if len (args) != 1:
        parser.print_help ()
        sys.exit (2)
    return options, args

def read_midi (file):
    import midi
    return midi.parse (open (file).read ())

def track_info (data):
    tracks = data[1]
    def track_name (track):
        name = ''
        for time, event in track:
            if time > 0:
                break
            if event[0] == 255 and event[1] == 3:
                name = event[2]
                break
        return name
    track_info = []
    for i in range (len (tracks)):
        track_info.append ((i, track_name (tracks[i])))
    return track_info


class formatter:
   def __init__ (self, txt = ""):
     self.text = txt
   def format_vals (self, val1, val2 = ""):
     return str (val1) + str(val2)
   def format (self, val1, val2 = ""):
     return self.text + self.format_vals (val1, val2)
class none_formatter (formatter):
   def format_vals (self, val1, val2 = ""):
     return ''
class meta_formatter (formatter):
   def format_vals (self, val1, val2):
     return str (val2);
class tempo_formatter (formatter):
   def format_vals (self, val1, val2):
    return str (ord (val2[0])*65536 + ord (val2[1])*256 + ord (val2[2])) \
        + " msec/quarter"

class time_signature_formatter (formatter):
   def format_vals (self, val1, val2 = ""):
       from fractions import Fraction
       # if there are more notated 32nd notes per midi quarter than 8,
       # we display a fraction smaller than 1 as scale factor.
       r = Fraction(8, ord (val2[3]))
       if r == 1:
           ratio =""
       else:
           ratio = " *" + str (r)
       return str (ord (val2[0])) + "/" + str(1 << ord (val2[1])) + ratio \
           + ", metronome "  + str (Fraction (ord (val2[2]), 96))
class key_signature_formatter (formatter):
   def format_vals (self, val1, val2):
       key_names = ['F', 'C', 'G', 'D', 'A', 'E', 'B']
       key = (((ord(val2[0])+128)%256)-128) + ord(val2[1])*3 + 1;
       return (key_names[key%7] + (key/7) * "is" + (-(key/7)) * "es"
               + " " + ['major','minor'][ord(val2[1])])
class channel_formatter (formatter):
   def __init__ (self, txt, ch):
     formatter.__init__ (self, txt)
     self.channel = ch
   def format (self, val1, val2 = ""):
     return self.text + "Channel " + str (self.channel) + ", " + \
            self.format_vals (val1, val2)
class control_mode_formatter (formatter):
   def __init__ (self, txt, ch):
     formatter.__init__ (self, txt)
     self.mode = ch
   def format (self, val1, val2 = ""):
     return self.text + str (self.mode) + ", " + \
            self.format_vals (val1, val2)
class note_formatter (channel_formatter):
   def pitch (self, val):
     pitch_names = ['C', 'Cis', 'D', 'Dis', 'E', 'F', 'Fis', 'G', 'Gis', 'A', 'Ais', 'B'];
     p = val % 12;
     oct = val / 12 -1;
     return pitch_names[p] + str(oct) + "(" + str(val) + ")"
   def velocity (self, val):
     #01   #10   #20   #30   #40   #50   #60   #70   #7F
     pass;
   def format_vals (self, val1, val2):
     return self.pitch (val1)


meta_dict = {0x00: meta_formatter ("Seq.Nr.:    "),
             0x01: meta_formatter ("Text:       "),
             0x02: meta_formatter ("Copyright:  "),
             0x03: meta_formatter ("Track name: "),
             0x04: meta_formatter ("Instrument: "),
             0x05: meta_formatter ("Lyric:      "),
             0x06: meta_formatter ("Marker:     "),
             0x07: meta_formatter ("Cue point:  "),
             0x2F: none_formatter ("End of Track"),
             0x51: tempo_formatter ("Tempo:      "),
             0x54: meta_formatter ("SMPTE Offs.:"),
             0x58: time_signature_formatter ("Time signature: "),
             0x59: key_signature_formatter ("Key signature: ")
}

def dump_event (ev, time, padding):
    ch = ev[0] & 0x0F;
    func = ev[0] & 0xF0;
    f = None
    if (ev[0] == 0xFF):
        f = meta_dict.get (ev[1], formatter ())
    if (func == 0x80):
        f = note_formatter ("Note off: ", ch)
    elif (func == 0x90):
        if (ev[2] == 0):
          desc = "Note off: "
        else:
          desc = "Note on: "
        f = note_formatter (desc, ch)
    elif (func == 0xA0):
        f = note_formatter ("Polyphonic aftertouch: ", ch, "Aftertouch pressure: ")
    elif (func == 0xB0):
        f = control_mode_formatter ("Control mode change: ", ch)
    elif (func == 0xC0):
        f = channel_formatter ("Program Change: ", ch)
    elif (func == 0xD0):
        f = channel_formatter ("Channel aftertouch: ", ch)
    elif (ev[0] in [0xF0, 0xF7]):
        f = meta_formatter ("System-exclusive event: ")

    if f:
      if len (ev) > 2:
        print padding + f.format (ev[1], ev[2])
      elif len (ev) > 1:
        print padding + f.format (ev[1])
      else:
        print padding + f.format ()
    else:
      print padding + "Unrecognized MIDI event: " + str (ev);

def dump_midi (data, midi_file, options):
    if not options.pretty:
        print data
        return
    # First, dump general info, #tracks, etc.
    print "Filename:     " + midi_file;
    i = data[0];
    m_formats = {0: 'single multi-channel track',
                 1: "one or more simultaneous tracks",
                 2: "one or more sequentially independent single-track patterns"}
    print "MIDI format:  " + str (i[0]) + " (" + m_formats.get (i[0], "") + ")";
    print "Divisions:    " + str (i[1]) + " per whole note";
    print "#Tracks:      " + str ( len (data[1]))
    n = 0;
    for tr in data[1]:
      time = 0;
      n += 1;
      print
      print "Track " + str(n) + ":"
      print "    Time 0:"
      for ev in tr:
        if ev[0]>time:
           time = ev[0]
           print "    Time " + str(time) + ": "
        dump_event (ev[1], time, "        ");



def go ():
    options, args = process_options (sys.argv[1:])
    midi_file = args[0]
    midi_data = read_midi (midi_file)
    info = track_info (midi_data)
    if (options.dump or options.pretty):
        dump_midi (midi_data, midi_file, options);
    elif options.regexp:
        import re
        regexp = re.compile (options.regexp)
        numbers = [str(n+1) for n, name in info if regexp.search (name)]
        if numbers:
            if options.prefix:
                sys.stdout.write ('%s ' % (options.prefix,))
            import string
            sys.stdout.write (string.join (numbers, ','))
            sys.stdout.write ('\n')
    else:
        for n, name in info:
            sys.stdout.write ('%d %s\n' % (n+1, name,))

if __name__ == '__main__':
    go ()
