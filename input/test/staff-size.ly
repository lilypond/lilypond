#(ly:set-option 'old-relative)
\version "2.1.6"

\header { texidoc = "@cindex Staff Size

Setting staff sizes is a little clumsy.  There are two options: using
@code{StaffContainer} and override/revert, or
@code{\applyoutput}. Both methods are shown in this example."

}

\score {
  \notes \relative c' <<
      \new StaffContainer {
	  \property StaffContainer.StaffSymbol \set #'staff-space = #(/ (* magstep magstep))
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
	  StaffSymbol \set #'staff-space = #(/ magstep)
      } {
	  \clef bass
	  c8 c c c  c c c c
      }
  >>
  \paper { raggedright = ##t}
}


