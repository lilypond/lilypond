\version "1.5.24.rz2"

\header{
texidoc="
This shows how accidentals are handled.
"
}

mel = \notes { \key d \major \time 4/4
 d4  dis dis8 dis, d4 | d dis disis8 d, dis4 | d des disis8 dis, d4 | dis deses d dis ~ | dis dis ~ dis8 d, dis4 ~ | \break
 dis dis cis c | c cis cisis cis | c ces cisis c | cis ceses c cis ~ | cis cis ~ cis cis \bar "|."  | \break
}

\score { \notes \context Staff \transpose c''' {
   \mel
   \property Score.oneMeasureLazy = ##t
   \property Score.autoAccidentals = #'(same-octave)
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'(same-octave)" \mel >
   \property Score.autoAccidentals = #'(lazy-same-octave)
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'(lazy-same-octave)" \mel >
   \property Score.autoAccidentals = #'(any-octave)
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'(any-octave)" \mel >
   \property Score.autoAccidentals = #'(lazy-any-octave)
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'(lazy-any-octave)" \mel >
   \stoneAccidentals
   < s1^"$\\backslash stoneAccidentals" \mel >
   \stoneCautionaries
   < s1^"$\\backslash stoneCautionaries" \mel >
   \noResetKey
   < s1^"$\\backslash noResetKey" \mel >
  }
  \paper {
    indent = 0.0
  }
}

