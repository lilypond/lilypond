\version "2.19.21"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "systems run off the page due to improper paper settings, setting default values"))

\header {
  texidoc = "Normally, margin settings must not cause systems to run off the page."
}

#(set-default-paper-size "a4")

someNotes = \relative { \repeat unfold 40 { c'4 d e f } }

\paper {
  left-margin = 20 \mm
  line-width = 200 \mm
}

\book {
  \score { \someNotes }
}
