\version "2.25.14"

\header {
  texidoc = "The @code{Span_bar_stub_engraver} also works with
@code{ChordNames} contexts, ensuring that chord names remain clear
of span bars."
}

\new StaffGroup <<
  \new Staff \relative { c''4 c c c | c c c c }
  \new ChordNames \chordmode { c2. q8. des16:maj9 | q1 }
  \new Staff { \improvisationOn b'4 4 4 8. 16~ | 4 4 4 4 }
>>
