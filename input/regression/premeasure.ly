\version "2.27.2"

\header {
  texidoc = "This tests the equivalence of @code{\\premeasure @var{music}} and
@code{\\partial @var{duration} @var{music}}.  The two scores should look
identical.

Only the final beat of the measure is beamed.  Measure@tie{}1 is missing
beat@tie{}3.  Measure@tie{}3 has an extra beat."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    %% Prevent beams in the first three beats.  Allow beaming in the fourth
    %% beat.  Let beaming continue into an extra beat, which we should never
    %% see.
    \overrideTimeSignatureSettings 4/4 1/8 1,1,1,1,1,1,4 #'()
  }
}

\score {
  \new Staff \with { instrumentName = "\\partial" } \fixed c' {
    \partial 4 c8 c |            % before measure 1
    d2 \partial 4 e8 e |         % skipping a beat
    f2. \partial 4 g8 g |        % not skipping, not extending
    a2. \partial 2 b8 b b b |    % repositioning backward
  }
}

\score {
  \new Staff \with { instrumentName = "\\premeasure" } \fixed c' {
    \premeasure { c8 c }         % before measure 1
    d2 \premeasure { e8 e }      % skipping a beat
    f2. \premeasure { g8 g }     % not skipping, not extending
    a2. \premeasure { b8 b b b } % repositioning backward
  }
}
