\version "2.23.8"

\header {
  texidoc = "At line break a broken @code{DurationLine}, like @code{Glissando},
avoids items with @code{break-aligned-interface}, like @code{KeySignature},
@code{BreathingSign} etc., but not items with the
@code{break-alignable-interface}, like @code{RehearsalMark},
@code{MetronomeMark}, etc.."
}

\layout {
  \context {
    \Score
    \override RehearsalMark.break-visibility = ##(#t #f #f)
    \override RehearsalMark.self-alignment-X = #RIGHT
  }
  \context {
    \Voice
    \consists "Duration_line_engraver"
    \omit Stem
    \omit Flag
    \omit Beam
    \override NoteHead.duration-log = 2
    \override Glissando.breakable = ##t
  }
}

<<
  \new Staff {
    b1\-\glissando
    \break
    \mark "long mark"
    \tempo "Allegretto, ma non troppo"
    \breathe
    s
    \break
    \key cis \major
    \time 2/2
    \clef "alto"
    s
    b'
  }
  \new Staff {
    b1\-\glissando
    \break
    s
    \break
    s
    b'
  }
>>
