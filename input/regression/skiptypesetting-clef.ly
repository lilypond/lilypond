\version "2.24.1"

\header {
  texidoc = "After a cut made with @code{skipTypesetting}, a change clef may
appear even if one would not normally appear there.  The requirement is loose:
it must simply be clear which clef the following music requires.  The expected
clef of the final measure appears in the margin."
}

%% change and then change back
\new Staff \with { instrumentName = "treble" } \fixed c' {
  c1
  \set Score.skipTypesetting = ##t
  \clef "bass"
  d1
  \clef "treble"
  d1
  \set Score.skipTypesetting = ##f
  c1
}

%% simple change
\new Staff \with { instrumentName = "bass" } \fixed c' {
  c1
  \set Score.skipTypesetting = ##t
  \clef "bass"
  d1
  \set Score.skipTypesetting = ##f
  c1
}

%% resume at a point that normally requires a guard clef, where the guard clef
%% also differs from the clef that was in effect before skipping began
\new Staff \with { instrumentName = "alto" } \fixed c' {
  c1
  \set Score.skipTypesetting = ##t
  d1
  \repeat volta 2 {
    \clef "bass"
    d1
    \clef "alto"
    d1
    \set Score.skipTypesetting = ##f
  }
  c1
}
