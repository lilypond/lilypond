#(ly:set-option 'old-relative)
\version "2.1.7"

\header { texidoc = "@cindex Staff Size

For setting staff sizes, both @code{staff-space} and @code{fontSize}
must be set."

}

\score {
  \notes \relative c' <<
      \new Staff \relative c'' {
	  \dynamicDown c,,4 \ff c c c
      }
      \new Staff \with {
	  fontSize = #-1
	  StaffSymbol \set #'staff-space = #(magstep -1)
      } {
	  \clef bass
	  c8 c c c  c c c c
      }
  >>
  \paper { raggedright = ##t}
}


