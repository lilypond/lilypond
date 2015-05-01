\version "2.19.20"

\header {
  texidoc = "This shows the single and multi voice rest positions for
various standard and tab staffs."
}


\layout {
  ragged-right = ##t
}

mus = { \mark "R1*7" R1*7 \mark "R1" R1 \mark "r1" r1 \mark "r2" r2 \mark "r4" r4*2 }

\score {
  {
    \compressMMRests
    \new StaffGroup <<
      $@(map
         (lambda (n)
           #{
    	 \new Staff \with { \override StaffSymbol.line-count = $n }
    	 { \mus << \mus \\ \mus >> }
           #})
         (iota 8))
      $@(map
          (lambda (x)
            #{
	      \new TabStaff \with { stringTunings = #x }
	      { \mus << \mus \\ \mus >> }
	    #})
          (list mandolin-tuning banjo-c-tuning guitar-tuning))
    >>
  }
  \layout { \tabFullNotation }
}
