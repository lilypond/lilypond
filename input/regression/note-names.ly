\version "2.13.38"

\header {
  texidoc = "Various languages are supported for note names input.
Selecting another language within a music expression is possible,
and doesn't break point-and-click abilities.
"
}


%% Old syntax.
\include "english.ly"

\relative c'' {
  g4 bf d c

  %% Manual override of the pitchnames variable
  %% and the parser note names:
  #(begin
    (set! pitchnames (ly:assoc-get 'nederlands language-pitch-names))
    (ly:parser-set-note-names parser pitchnames))
  bes4 a g fis

  %% The \language command acts in the same way:
  \language "italiano"
  sol4 fa mib re
}
