#(ly:set-option 'old-relative)
\version "1.9.4"

\header { texidoc = "@cindex Staff Size

Setting staff sizes is a little clumsy.  There are two options: using
@code{StaffContainer} and override/revert, or
@code{\outputproperty}. Both methods are shown in this example. "

}

\score {
  \notes \relative c' << \new StaffContainer {
     \property StaffContainer.StaffSymbol \set #'staff-space = #(/ 16 20)

	\property Staff.fontSize = #-1
	\property Voice.fontSize = #-1
	
	\dynamicUp\stemDown

	%\key gis \major
	c8 d  e[ f g a] b c \ff
  }

\new Staff \relative c'' { \dynamicDown c,,4 \ff c c c  }
\new Staff {
  \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
    #'staff-space =  #0.8
  \property Staff.fontSize = #-1
  \clef bass
  c8 c c c  c c c c
}
>>
\paper { raggedright = ##t}
}


