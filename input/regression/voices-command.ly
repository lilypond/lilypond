\version "2.19.59"

\header {
  texidoc = "The @code{\\voices} command can be used for continuing voices
and changing the order of @code{\\voiceOne}@dots{}@code{\\voiceFour} style
overrides."
}

\layout { ragged-right = ##t }

{
  \time 2/4
  \new Voice = "sop" {
    a'2~ |
    \voices 1,"sop" << e''2 \\ { \voiceTwo a'2~ \oneVoice } >>
    a'2
  }
  \voices 1,3,4,2 << a''2 \\ e''2 \\ c''2 \\ a'2 >>
}
