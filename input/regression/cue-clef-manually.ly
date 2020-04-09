\version "2.19.21"

\header {
  texidoc = "
Cue clefs can be printed manually.
"
}

Solo = \relative {
  c'4 c c c |

  % Manually written cue notes, not quoted from another lilypond voice:
  <<
    { \voiceTwo R1 \oneVoice }
    \new CueVoice
    {
      \cueClef "bass"
      \voiceOne
      c4 c c c |
      \cueClefUnset
    }
  >>
  c4 c c c |
}

\score {
  <<
    \new Staff \Solo
  >>
}
