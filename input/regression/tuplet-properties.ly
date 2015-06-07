\version "2.19.21"

\header {
  texidoc = "Tuplet bracket formatting supports numerous options,
for instance, bracketed (B) and non-bracketed (NB).
"
}

\context Voice \relative {

  \tuplet 3/2 { c'''8 c,, c }
  \tuplet 3/2 { c'8 c'' c,, }

  \tuplet 3/2 { c8[^"NB" c c] }

  \tuplet 3/2 { c8^"B"  c[ c] }
  \tuplet 4/2 { r8_"B"  c,[ c'] r8 }

  \override TupletBracket.bracket-visibility = #'if-no-beam  
  \tuplet 3/2 { c8[ c c] }

  \tupletUp
  \omit TupletNumber
  \tuplet 3/2 { c8^"up, no digit" c[ c] }
  \undo \omit TupletNumber

  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.edge-height = #'(0 . 0)
  \override TupletBracket.shorten-pair = #'(2.0 . 2.0)
  \tuplet 6/4 { c_"shorter, no edges" f b  b f c}	
  \revert TupletBracket.edge-height
  \revert TupletBracket.shorten-pair

  \override TupletBracket.bracket-flare = #'(0.5 . 0.5)
  \tuplet 3/2 { b^"angled edges" b b }
  \tupletNeutral
  \tuplet 3/2 { b b b }
}
