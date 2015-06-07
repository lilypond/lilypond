\header { texidoc = "A heavy-bar system start delimiter may be created by tuning the @code{SystemStartBar} grob."
	} 

\version "2.19.21"

\paper {
  ragged-right = ##t
}
<<
  \new StaffGroup
  \relative <<
    \set StaffGroup.systemStartDelimiter = #'SystemStartBar
    \override StaffGroup.SystemStartBar.thickness = #8
    \override StaffGroup.SystemStartBar.padding = #0.2
    \new Staff { c'1 }
    \new Staff { c1 }
  >>
  \new Staff { c1 }
>>
