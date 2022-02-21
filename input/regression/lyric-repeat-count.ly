\version "2.23.7"

\header {
  texidoc = "This tests the default appearance of repeats for modern
transcriptions of Gregorian chant.  The repeat count appears in the
lyric line under the finalis sign (double line) that ends the repeated
section, even if the repeat count is 1.  The count is an italicized
lowercase roman number followed by a period.  A final ``i'' is
replaced by ``j''."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
}

music = \fixed c' {
  \repeat volta 2 { f2 g }
  \repeat volta 3 { a g }
  \repeat volta 4 { f e }
  \repeat volta 1 f1
  g\breve f1
}

words = \lyricmode {
  \repeat volta 2 { Lo2 -- rem. }
  \repeat volta 3 { Ip -- sum. }
  \repeat volta 4 { Do -- lor. }
  \repeat volta 1 Sit.1
  A\breve -- met.1
}

piece = <<
  \new GregorianTranscriptionStaff \music
  \new GregorianTranscriptionLyrics \words
>>

\score { \piece }
\score { \unfoldRepeats \piece }
