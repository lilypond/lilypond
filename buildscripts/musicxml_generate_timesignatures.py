#!/usr/bin/env python

notes = "CDEFGAB"
alterations = [-1, 0, 1]

def print_measure (nr, beats, type, params = "", attr = "", barline = ""):
    print """    <measure number="%s">
      <attributes>
%s        <time %s>
          <beats>%s</beats>
          <beat-type>%s</beat-type>
        </time>
      </attributes>
      <note>
        <pitch>
          <step>C</step>
          <octave>5</octave>
        </pitch>
        <duration>1</duration>
        <voice>1</voice>
        <type>quarter</type>
      </note>
%s    </measure>""" % (nr, attr, params, beats, type, barline)

first_atts = """        <divisions>1</divisions>
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
"""

final_barline = """      <barline location="right">
        <bar-style>light-heavy</bar-style>
      </barline>
"""

print """<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 1.0 Partwise//EN"
                                "http://www.musicxml.org/dtds/partwise.dtd">
<score-partwise>
  <movement-title>Different time signatures</movement-title>
  <part-list>
    <score-part id="P1">
      <part-name>MusicXML Part</part-name>
    </score-part>
  </part-list>
  <!--=========================================================-->
  <part id="P1">
"""

measure = 1

print_measure (measure, 2, 2, " symbol=\"common\"", first_atts)
measure += 1

print_measure (measure, 4, 4, " symbol=\"common\"")
measure += 1

print_measure (measure, 2, 2)
measure += 1

print_measure (measure, 3, 2)
measure += 1

print_measure (measure, 2, 4)
measure += 1

print_measure (measure, 3, 4)
measure += 1

print_measure (measure, 4, 4)
measure += 1

print_measure (measure, 5, 4)
measure += 1

print_measure (measure, 3, 8)
measure += 1

print_measure (measure, 6, 8)
measure += 1

print_measure (measure, 12, 8, "", "", final_barline)
measure += 1

print """  </part>
</score-partwise>
"""
