\version "2.3.4"

\header { texidoc = "@cindex Staff Size

In order to change staff sizes, both @code{staff-space} and @code{fontSize}
must be scaled."

}

\score {
   \relative c' <<
      \new Staff \relative c'' {
	  \dynamicDown c,,4 \ff c c c
      }
      \new Staff \with {
	  fontSize = #-1
	  \override StaffSymbol #'staff-space = #(magstep -1)
      } {
	  \clef bass
	  c8 c c c  c c c c
      }
  >>
  \paper { raggedright = ##t}
}


