\version "2.15.9"

\header {
  texidoc = "
This tests various segfault fixes from merging previously-separate
regtests together.  The output is not important; all that matters
is that it compiles.

Hopefully we can separate the regtests again in the future.
"
}
% merged to avoid
% http://code.google.com/p/lilypond/issues/detail?id=1821


%%% book-label-no-segfault.ly
% A book(part) can contain only a label without causing a segfault.
\book {\markup "foo"}   % necessary to produce some output
\book { \label #'foo }


%%% metronome-multimeasure-rest-no-segfault.ly
% A metronome marking can be added to a multimeasure rest whose
% engraver was moved to the Staff, without segfaulting.
\score {
  \new Staff {
    \tempo 4=150
    R1 |
  }
  \layout {
    \context {
      \Score
      \remove "Metronome_mark_engraver"
      \remove "Staff_collecting_engraver"
    }
    \context {
      \Staff
      \consists "Metronome_mark_engraver"
    }
  }
}


%%% ambitus-with-ligature.ly
% A @code{\Voice} should be able to contain both an
% @code{Ambitus_engraver} and a @code{Mensural_ligature_engraver}
% without segfaulting.
\new Voice \with  {
  \consists Ambitus_engraver
  \consists Mensural_ligature_engraver
} {
  \[ c'\longa c''\longa \]
}


%%% beam-skip.ly
% Beams over skips do not cause a segfault.
\new Voice << { c'4 c'8 c' } { s8[ s] s[ s] } >>


%%% bookpart-variable.ly
% A @code{\bookpart} variable can be inserted in a @code{\book}.
% No segfault should occur in this case.
mypart = \bookpart {
  \relative c' {
    c1
  }
}
\book {
  \mypart
}


%%% skiptypesetting-all-true.ly
% A score with @code{skipTypesetting} set for the whole score
% will not segfault.
{
  \set Score.skipTypesetting = ##t
  c'4
}


%%% skiptypesetting-multimeasurerest.ly
% When @code{skipTypesetting} is set during a
% @code{skipBars}-induced @code{MultiMeasureRest} spanner, no
% segfault occurs.
<<
  {
    \time 3/4
    \set Score.skipBars = ##t
    a4 a a
    R2.*2
  }
  \\
  {
    \set Score.skipTypesetting = ##t
    s2. s4
    \set Score.skipTypesetting = ##f
  }
>>




