\header  {
  texidoc = "Specifying @code{grow-direction} on a beam causes feathered
beaming.  The @code{\\featherDurations} function can be used to adjust note
durations and the duration of unassigned lyrics."

}

\version "2.25.28"

\paper {
  ragged-right = ##t
  indent = #0.0
}

%
% Unfortunately this quickly bumps into overflow problems. Should use
% a linear decrease instead?
%

<<
  \featherDurations 3/4
  \relative {
    \override Beam.grow-direction = #LEFT
    c''16[ c c c  c c c c]
  }
  \new Lyrics
    \lyricmode {
      \set stanza = "adjusted"
      \featherDurations 3/4 { x x x x  x x x x }
    }
  \new Lyrics
    \lyricmode {
      \set stanza = " not adjusted:"
      x x x x  x x x x
    }
>>
