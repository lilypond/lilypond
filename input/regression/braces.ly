\version "2.15.6"

\header{
  texidoc="
Braces can be used to show organ keyboard changes.
"
}

\score {
  <<
    \new PianoStaff <<
      { << { d''2~\brace d''~ d'' } \\ { s1 <a d' f'>2\brace a' } >> }
      \new Dynamics { s1-\markup \bold \upright "G.O." }
      { f8\brace
        \once \override Brace #'minimum-brace-height = #1
        \once \override Brace #'positions = #'(-5.5 . 0)
        <a' c' e'>\brace-\markup \bold "Pos." a' a' a'2\brace f'\brace f' }
    >>
    \new Staff { \clef F R1 d2\brace d }
  >>
  \layout {
    ragged-right = ##t
    \context {
      \Score
      \consists Span_brace_engraver
      connectBraces = ##t
    }
  }
}
