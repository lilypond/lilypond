\version "2.23.0"

\header {
  texidoc = "@code{BendSpanner} may be started at a tied note.
To skip tied notes @code{NoteColumn.bend-me} should be set to false.
The following @code{BendSpanner} continues without a gap."
}

bends-with-ties-and-skips = {
  a'4~\^ \skipNC a'4~ \skipNC a'4 b'4
  a'4~ a'4~\^ \skipNC a'4 b'4
  a'4~ a'4~ a'4\^ b'4
  c'2\^ d'~ \bendHold \^ \skipNC d'~ d'\^ c'
  \grace { c'8-\preBendHold \^ } \skipNCs d'2~ d'2~ \endSkipNCs d'\^ c'2
  \bar "|."
}

\score {
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \bends-with-ties-and-skips }
    \new TabVoice \bends-with-ties-and-skips
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
    \context {
      \TabStaff
      minimumFret = #3
      restrainOpenStrings = ##t
    }
  }
}
