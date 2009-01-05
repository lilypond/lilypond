#!@PYTHON@

import sys
import midi

(h,tracks) = midi.parse (open (sys.argv[1]).read ())

tracks = tracks[1:]

for t in tracks:
    for e in t:
        print e
