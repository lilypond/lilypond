#(ly:set-option 'old-relative)
\version "2.1.6"

\header { texidoc = "@cindex Staff Size

For setting staff sizes there are two options: using
@code{StaffContainer} and override/revert, or
@code{\with}. Both methods are shown in this example."

}

\score {
  \notes \relative c' <<
      \new StaffContainer {
	  \property StaffContainer.StaffSymbol \set #'staff-space = #(magstep -2)
	  \property Staff.fontSize = #-2
	
	  \dynamicUp\stemDown
	  
				%\key gis \major
	  c8 d  e[ f g a] b c \ff
      }

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


