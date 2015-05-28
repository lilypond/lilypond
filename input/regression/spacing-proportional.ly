\header {

  texidoc = "Proportional notation can be created by setting
@code{proportionalNotationDuration}. Notes will be spaced proportional
to the distance for the given duration."

}

\version "2.19.21"

\paper { ragged-right = ##t }

\relative
<<
  \set Score.proportionalNotationDuration = #(ly:make-moment 1/16)
  \new Staff { c''8[ c c c c c]  c4 c2 r2 }
  \new Staff { c2  \tuplet 3/2 { c8 c c } c4 c1 }
>>

   
	 

  
