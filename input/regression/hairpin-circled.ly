\version "2.19.21"

\header {

  texidoc = "Hairpins can have circled tips. A decrescendo del niente
followed by a crescendo al niente should only print one circle."

}

\layout { ragged-right = ##t } 

\relative {
  \override Hairpin.circled-tip = ##t
  c''1\< d\! d\> c\!
  \override Hairpin.to-barline = ##f
  e\> c\< d\! \break
  c\< \break
  e d\! c\> \break
  e d\!
}

