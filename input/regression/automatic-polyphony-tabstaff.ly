\version "2.16.0"

\header{ texidoc = "In a TabStaff, automatic polyphony can be used without
                    explicitly initializing separate voices."
       }

test = {
  c'1
  << { c'4 d' e' f' } \\ { g,1 } >>
  c'1
}

\score {
  <<
    \new Staff { \clef "treble_8" \test }
    \new TabStaff { \test }
  >>
}
