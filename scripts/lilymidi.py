#!@TARGET_PYTHON@

# Copyright (c) 2006--2008 Brailcom, o.p.s.
#
# Author: Milan Zamazal <pdm@brailcom.org>
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.


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

def go ():
    options, args = process_options (sys.argv[1:])
    midi_file = args[0]
    midi_data = read_midi (midi_file)
    info = track_info (midi_data)
    if options.dump:
        print midi_data
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
