\header {

  texidoc = "Proportional notation can be created by setting
@code{proportionalNotationDuration}. Notes will be spaced proportional
to the distance for the given duration."

}

\version "2.12.0"

\paper { ragged-right = ##t }

\relative c''
<<
  \set Score.proportionalNotationDuration = #(ly:make-moment 1 16)
  \new Staff { c8[ c c c c c]  c4 c2 r2 }
  \new Staff { c2  \times 2/3 { c8 c c } c4 c1 }
>>

   
	 

  
