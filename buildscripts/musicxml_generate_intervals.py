#!/usr/bin/env python

notes = "CDEFGAB"
alterations = [-1, 0, 1]

def print_note (octave, note, alteration):
    print "      <note>\n        <pitch>\n          <step>%s</step>" % notes[note]
    if alteration <> 0:
        print "          <alter>%s</alter>" % alteration
    print "          <octave>%s</octave>\n        </pitch>\n        <duration>1</duration>\n        <voice>1</voice>\n        <type>quarter</type>\n      </note>" % octave


print """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 1.0 Partwise//EN"
                                "http://www.musicxml.org/dtds/partwise.dtd">
<score-partwise>
  <movement-title>Various piches and interval sizes</movement-title>
  <part-list>
    <score-part id="P1">
      <part-name>MusicXML Part</part-name>
    </score-part>
  </part-list>
  <!--=========================================================-->
  <part id="P1">
    <measure number="1">
      <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
          <mode>major</mode>
        </key>
        <time symbol="common">
          <beats>2</beats>
          <beat-type>4</beat-type>
        </time>
        <clef>
          <sign>G</sign>
          <line>2</line>
        </clef>
      </attributes>
"""

start_octave = 5

for octave in (start_octave, start_octave+1):
    for note in (0,1,2,3,4,5,6):
        for alteration in alterations:
            if octave == start_octave and note == 0 and alteration == -1:
                continue
            print_note (octave, note, alteration)
#             if octave == start_octave and note == 0 and alteration == 0:
#                 continue
            print_note (start_octave-(octave-start_octave)-(1-(7-note)/7), (7-note)%7, -alteration)

print """    </measure>
  </part>
</score-partwise>
"""
