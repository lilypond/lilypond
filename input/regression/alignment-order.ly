\header {
  texidoc = "Newly created contexts can be inserted
anywhere in the vertical alignment. "
}

\version "2.7.32"

\paper {
  ragged-right = ##t
}

\relative <<
  \context Staff = "1" { c4 c s2 }
  \context Staff = "2" { c4  c s2 }
  \context Staff = "3" { c4  c s2 }
  { \skip 2
    <<
      \lyrics {
	\set alignBelowContext = #"1"
	below8 first staff
      }
      \new Staff {
	\set Staff.alignAboveContext = #"3"
	\times 4/6 {
	  \override TextScript #'padding = #3
	  c8^"this" d_"staff" e^"above" d_"last" e^"staff" f
	}
      }
    >> }
>>
