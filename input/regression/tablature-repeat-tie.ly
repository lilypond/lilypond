\version "2.25.19"

\header {
  texidoc = "If a @code{RepeatTie} occurs, the corresponding fret number is
always displayed in parentheses."
}

mus = {
  \repeat volta 2 {
    b2\repeatTie
    <b d' f'>\repeatTie
    <b\repeatTie d'\repeatTie >
    <b d'^\repeatTie >
    <b_\repeatTie d' >
    b2~
    \alternative { b1 b\repeatTie }
  }
}

\score {
  <<
    \new Staff \mus
    \new Lyrics
      \lyricmode {
        "(0)"2
        \markup \column \parenthesize { 1 3 4 }
        \markup \column \parenthesize { 3 4 }
        \markup \column  { \parenthesize 3 4 }
        \markup \column  { 3 \parenthesize 4 }
        "0"
        "/"1
        "(0)"
      }
    \new TabStaff
      \with { instrumentName = "default" }
      \mus
    \new Lyrics
      \lyricmode {
        "/"2 "/" "/" "4" "3" "0" "/"1 "/"
      }
    \new TabStaff
      \with { instrumentName = "hideSplitTiedTabNotes" \hideSplitTiedTabNotes }
      \mus
    \new TabStaff
      \with {
        instrumentName = "tabFullNotation"
        \hideSplitTiedTabNotes
        \tabFullNotation
      }
      \mus
  >>
  \layout {
    indent = 3\cm
    \context {
      \Lyrics
      \override LyricText.font-size = #-4
      \override LyricText.baseline-skip = 1
      instrumentName = \markup \fontsize #-2 "below should print:"
      \override VerticalAxisGroup.staff-affinity = #CENTER
    }
  }
}
