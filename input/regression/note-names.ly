
\header
{
  texidoc = "Various languages are supported for note names input.
Selecting another language within a music expression is possible,
and doesn't break point-and-click abilities.
"
}

\version "2.13.37"

\include "english.ly"

\relative c'' {
  g bf d c

  %% nederlands.ly is loaded by default, so the
  %% dutchPitchnames variable is already defined.
  #(begin
    (set! pitchnames dutchPitchnames)
    (ly:parser-set-note-names parser dutchPitchnames))
  bes a g fis

  %% The \language command just retrieves a pitches alist
  %% from the relevant file, and sets pitchnames accordingly.
  \language "italiano"
  sol fa mib re
}