\version "2.16.0"

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

music = \relative c'' { \key fis \minor c1 d e }

\score { <<
  % No modifications:
  \new Staff { \music }
  % Some context modifications manually written in a \with block
  \new Staff \with {
    \remove "Time_signature_engraver"
    \consists "Ambitus_engraver"
    \override StaffSymbol #'line-count = 4
  } { \music }
  % The same mods as direct value of \with
  \new Staff \with \ctxmod { \music }
  % Mods as part of a \with block
  \new Staff \with { \ctxmod } { \music }
  % Mods before a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
  } { \music }
  % Mods before and after a context mod in a with block are working:
  \new Staff \with {
    \remove "Clef_engraver"
    \ctxmod
    \remove "Key_engraver"
  } { \music }
  % Mods can be inserted instead of a \with block (i.e. \with is not required)
  \new Staff \ctxmod { \music }
  \new Staff { \music }
>>
}
