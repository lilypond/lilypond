\version "2.14.0"

\header  {
texidoc = "Context modifications can be stored into a variable as a
\with object. They can be later inserted into another \with block."
}

% Some sample modifications to be inserted into a \with block later on
ctxmod = \with {
  \remove "Time_signature_engraver"
  \consists "Ambitus_engraver"
  \override StaffSymbol #'line-count = 4
}

\layout {
  \context {
    \Score
    \remove "Mark_engraver"
    \remove "Staff_collecting_engraver"
  }
  \context {
    \Staff
    \consists "Mark_engraver"
    \consists "Staff_collecting_engraver"
  }
}


music = \relative c'' { \key fis \minor c1 d e }

\score { <<
  \override Score.RehearsalMark #'self-alignment-X = #LEFT
  \override Score.RehearsalMark #'font-size = #-2
  % No modifications:
  \new Staff { \mark \markup { No modifications } \music }
  \new Staff \with {
    \remove "Time_signature_engraver"
    \consists "Ambitus_engraver"
    \override StaffSymbol #'line-count = 4
  } {
    \mark
    \markup { "Remove time sig, add ambitus, set staff to 4 lines" }
    \music }
  % Some context modifications manually written in a \with block
  \new Staff \with \ctxmod {
    \mark \markup { "The same mods using a variable" } \music
  }
  % The same mods as direct value of \with
  \new Staff \with { \ctxmod } {
    \mark \markup { "The same mods using a variable and \with" }
    \music
  }
  % Mods before a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
  } {
    \mark
    \markup { "Remove clef and use variable to add other changes as above" }
    \music
  }
  % Mods before and after a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
    \remove "Key_engraver"
  } { \mark \markup { "Also remove clef and key engravers" } \music }
  % Mods can be inserted instead of a \with block (i.e. \with is not required)
  \new Staff \ctxmod { \mark \markup { "The same mods as staff 2" } \music }
  \new Staff { \mark \markup { "Back to default" } \music }
>>
}
