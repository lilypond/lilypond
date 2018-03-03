\version "2.19.46"

\header  {
texidoc = "Context modifications can be stored into a variable as a
\with object. They can be later inserted into another \with block."
}

% Some sample modifications to be inserted into a \with block later on
ctxmod = \with {
  \remove "Time_signature_engraver"
  \consists "Ambitus_engraver"
  \override StaffSymbol.line-count = 4
}

music = \relative { \key fis \minor c''1 d e }

\score { <<
  % No modifications:
  \new Staff { <>^\markup { No modifications } \music }
  \new Staff \with {
    \remove "Time_signature_engraver"
    \consists "Ambitus_engraver"
    \override StaffSymbol.line-count = 4
  } {
    <>^\markup { "Remove time sig, add ambitus, set staff to 4 lines" }
    \music }
  % The same mods as direct value of \with
  \new Staff \with \ctxmod {
    <>^\markup { "The same mods using a variable" } \music
  }
  % Some context modifications manually written in a \with block
  \new Staff \with { \ctxmod } {
    <>^\markup { "The same mods using a variable and \with" }
    \music
  }
  % Mods before a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
  } {
    <>^\markup { "Remove clef and use variable to add other changes as above" }
    \music
  }
  % Mods before and after a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
    \remove "Key_engraver"
  } { <>^\markup { "Also remove clef and key engravers" } \music }
  % Test rendered redundant by issue 4911
  \new Staff \with \ctxmod { <>^\markup { "The same mods as staff 2" } \music }
  \new Staff { <>^\markup { "Back to default" } \music }
>>
}
