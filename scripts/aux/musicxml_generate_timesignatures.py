#!/usr/bin/env python

notes = "CDEFGAB"
alterations = [-1, 0, 1]

dot_xml = """        <dot/>
"""
tie_xml = """        <tie type="%s"/>
"""
tie_notation_xml = """        <notations><tied type="%s"/></notations>
"""


def generate_note (duration, end_tie = False):
    if duration < 2:
      (notetype, dur) = ("8th", 1)
    elif duration < 4:
      (notetype, dur) = ("quarter", 2)
    elif duration < 8:
      (notetype, dur) = ("half", 4)
    else:
      (notetype, dur) = ("whole", 8)
    dur_processed = dur
    dot = ""
    if (duration - dur_processed >= dur/2):
      dot = dot_xml
      dur_processed += dur/2
      if (duration - dur_processed >= max(dur/4, 1)):
        dot += dot_xml
        dur_processed += dur/4
    tie = ""
    tie_notation = ""
    if end_tie:
        tie += tie_xml % "stop"
        tie_notation += tie_notation_xml % "stop"
    second_note = None
    if duration - dur_processed > 0:
        second_note = generate_note (duration-dur_processed, True)
        tie += tie_xml % "start"
        tie_notation += tie_notation_xml % "start"
    note = """      <note>
        <pitch>
          <step>C</step>
          <octave>5</octave>
        </pitch>
        <duration>%s</duration>
%s        <voice>1</voice>
        <type>%s</type>
%s%s      </note>""" % (dur_processed, tie, notetype, dot, tie_notation)
    if second_note:
        return "%s\n%s" % (note, second_note)
    else:
        return note

def print_measure (nr, beats, type, params = "", attr = "", attr2 = "", barline = ""):
    duration = 8*beats/type
    note = generate_note (duration)

    print """    <measure number="%s">
      <attributes>
%s        <time%s>
          <beats>%s</beats>
          <beat-type>%s</beat-type>
        </time>
%s      </attributes>
%s
%s    </measure>""" % (nr, attr, params, beats, type, attr2, note, barline)

first_key = """        <divisions>2</divisions>
        <key>
          <fifths>0</fifths>
          <mode>major</mode>
        </key>
"""
first_clef = """        <clef>
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
  <identification>
    <miscellaneous>
      <miscellaneous-field name="description">Various time signatures: 2/2 
            (alla breve), 4/4 (C), 2/2, 3/2, 2/4, 3/4, 4/4, 5/4, 3/8, 6/8, 
            12/8</miscellaneous-field>
    </miscellaneous>
  </identification>
  <part-list>
    <score-part id="P1">
      <part-name>MusicXML Part</part-name>
    </score-part>
  </part-list>
  <!--=========================================================-->
  <part id="P1">"""

measure = 1

print_measure (measure, 2, 2, " symbol=\"common\"", first_key, first_clef)
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

print_measure (measure, 12, 8, "", "", "", final_barline)
measure += 1

print """  </part>
</score-partwise>"""
