\version "2.16.0"

\header {
  texidoc = "Chords can be created using chord name entries (in
@code{\\chordmode} mode), using a pitch and a suffix."
}

Suffixes = \lyrics {
  "(nothing)"
  ":7"
  ":m"
  ":m7"

  ":aug"
  ":maj7"
  ":dim"
  ":dim7"

  ":sus4"
  ":sus2"
  ":6"
  ":m6"

  ":7sus4"
  ":3-"
  ":3+"
  ":5+.3-"

  ":7"
  ":9"
  ":11"
  ":13"

  ":m13"
  ":7^5"
  "^3"
  "/g"

  "/gis"
  "/a"
  "/+f"
  "/+g"
}

Chords = \chordmode {
  c
  c:7
  c:m
  c:m7

  c:aug
  c:maj7
  c:dim
  c:dim7 \break

  c:sus4
  c:sus2
  c:6
  c:m6

  c:7sus4
  c:3-
  c:3+
  c:5+.3- \break

  c:7
  c:9
  c:11
  c:13

  c:m13
  c:7^5
  c^3
  c/g

  c/gis
  c/a
  c/+f
  c/+g
}

<<
  \new ChordNames \Chords
  \new Voice \Chords
  \new Lyrics \Suffixes
>>
