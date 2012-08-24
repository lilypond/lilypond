\version "2.16.0"

Solo = \relative c' { 
  c4 c c c |

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
