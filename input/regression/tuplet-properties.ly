
\header {
  texidoc = "Tuplet bracket formatting supports numerous options,
for instance, bracketed (B) and non-bracketed (NB).
"
}


\version "2.11.51"



\context Voice \relative c'' {

  \times 2/3 { c'8 c,, c }
  \times 2/3 { c'8 c'' c,, }

  
  \times 2/3 {  c8[^"NB" c c]  }
  
  \times 2/3 { c8^"B"  c[ c]  }
  \times 2/4 { r8_"B"  c,[ c'] r8 }
  
  \override TupletBracket  #'bracket-visibility = #'if-no-beam  
  \times 2/3 {  c8[ c c]  }
  
  \tupletUp
  \override TupletNumber #'transparent = ##t
  \times 2/3 { c8^""^""^"up, no digit"  c[ c]  }
  \revert TupletNumber #'transparent

  \override TupletBracket  #'bracket-visibility = ##t
  \override TupletBracket  #'shorten-pair = #'(2.0 . 2.0)
  \times 4/6 { c_"shorter, no edges" f b  b f c}	
  \revert TupletBracket #'edge-height
  \revert TupletBracket #'shorten-pair
  \override TupletBracket  #'bracket-flare = #'(0.5 . 0.5)
  \times 2/3 { b^""^""^"angled edges" b b }
  \tupletNeutral
  \times 2/3 { b b b }

}


