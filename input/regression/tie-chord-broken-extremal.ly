\version "2.19.21"

\header { texidoc = "

Tie detail property multi-tie-region-size controls how many variations
are tried for the extremal ties in a chord.


" }

\relative {
  \time 4/4 <bis' bis>1 ~ \break

  \override Tie.details.multi-tie-region-size = #1
  \time 3/4 <bis bis>2.~ \break
  \time 4/4 <bis bis>1 
}

\paper {
  ragged-right = ##t
  debug-tie-scoring = ##t
}
