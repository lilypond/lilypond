\version "2.25.0"

\header {
  texidoc = "The option @code{span-all-note-heads} may be used to
make tuplet brackets span over all note heads (instead of stems) as
done in standard typesetting practice."
}

\layout {
  ragged-right = ##t
  \context {
    \Voice
    \override TupletBracket.span-all-note-heads = ##t
  }
}

{
  \tuplet 3/2
    {
      \tuplet 3/2
        {
          fis'8
          (
          e'8
          d'8
        }
      g'4
      f'4
      )
    }
  \tuplet 3/2
    {
      r8
      a8
      r8
    }
}

music = {
  \voiceOne
  \times 2/3 { <c>4 q q }
  \voiceTwo
  \times 2/3 { <c> q \bar "" \break q }
  \voiceOne
  \times 2/3 { <c d> q \bar "" \break q }
  \voiceTwo
  \times 2/3 { <c d> \bar "" \break q q }
}

\relative c'' {
  \music
}

\new RhythmicStaff {
  \set tupletFullLength = ##t
  \set tupletFullLengthNote = ##f
  \time 2/4
  \tuplet 3/2 { c4 4 4 }
  \time 4/4
  \tuplet 5/4 { 4 1 }
  \time 3/4
  2.
}

<<
  \new Staff {
    \tuplet 3/2 { c''4 4 4 } r2
  }
  \new Staff {
    \tuplet 3/2 { <c' d'>4 4 4 } r2
  }
>>

