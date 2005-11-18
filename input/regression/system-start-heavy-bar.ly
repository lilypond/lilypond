\header { texidoc = "A heavy-bar system start delimiter may be created by tuning the @code{SystemStartBar} grob."
	} 

\version "2.7.18"

\paper {
  raggedright = ##t
}
<<
  \new StaffGroup
  \relative <<
    \set StaffGroup.systemStartDelimiter = #'SystemStartBar
    \override StaffGroup.SystemStartBar #'thickness = #8
    \override StaffGroup.SystemStartBar #'padding = #0.2
    \new Staff { c1 }
    \new Staff { c1 }
  >>
  \new Staff { c1 }
>>
