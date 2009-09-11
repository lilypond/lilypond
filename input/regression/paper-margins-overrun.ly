\version "2.13.4"

#(ly:set-option 'warning-as-error #f)

\header {
  texidoc = "Normally, margin settings must not cause systems to run off the page."
}

#(set-default-paper-size "a4")

someNotes = \relative c' { \repeat unfold 40 { c4 d e f }}

\paper {
  left-margin = 20 \mm
  line-width = 200 \mm
}

\score { \someNotes }
