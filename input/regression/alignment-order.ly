\header {
  texidoc = "Newly created contexts can be inserted
anywhere in the vertical alignment. "
}

\version "2.5.23"

\paper {
  raggedright = ##t
}

\relative <<
  \context Staff = "1" { c4 c s2 }
  \context Staff = "2" { c  c s2 }
  \context Staff = "3" { c  c s2 }
  { \skip 2
    <<
      \lyrics {
	\set alignBelowContext = #"1"
	below8 first staff
      }
      \lyrics {
	\set alignAboveContext = #"3"
	above8 last staff
      }
    >> }
>>
