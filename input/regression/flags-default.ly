\version "2.11.57"
#(set-global-staff-size 17)

\header {
  texidoc = "Default flag styles: '(), 'mensural and 'no-flag.
  Compare all three methods to print them (C++ default implementation, 
  Scheme implementation using the 'flag-style grob property and 
  setting the 'flag property explicitly to the desired Scheme function.
  All three lines should be absolutely identical."
}

\paper {
  line-width = 18\cm
}

% test notes, which will be shown in different style:
testnotes = { \autoBeamOff c'8 d'16 c'32 d'64 \acciaccatura {c'8} d'64 c''8 d''16 c''32 d''64 \acciaccatura {c''8} d''64  }

{
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \time 2/4
  s2 \break

  % Old settings: default, 'mensural, 'no-flag
  \mark "Default flags (C++)"
  \testnotes

  \mark "Symbol: 'mensural (C++)"
  \override Stem #'flag-style = #'mensural
  \testnotes

  \mark "Symbol: 'no-flag (C++)"
  \override Stem #'flag-style = #'no-flag
  \testnotes

  \break

  % The same, but with the Scheme implementation of default-flag
  \override Stem #'flag = #default-flag
  \revert Stem #'flag-style
  \mark "Default flags (Scheme)"
  \testnotes

  \mark "Symbol: 'mensural (Scheme)"
  \override Stem #'flag-style = #'mensural
  \testnotes

  \mark "Symbol: 'no-flag (Scheme)"
  \override Stem #'flag-style = #'no-flag
  \testnotes

  \break

  % New settings: no settings, normal-flag, mensural-flag, no-flag
  \mark "Function: normal-flag"
  \override Stem #'flag = #normal-flag
  \testnotes

  \mark "Function: mensural-flag"
  \override Stem #'flag = #mensural-flag
  \testnotes

  \mark "Function: no-flag"
  \override Stem #'flag = #no-flag
  \testnotes
}
