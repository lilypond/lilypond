

\header { texidoc=

	  "The analysis of the part combiner is non-local:
in the following example, the decision for using separate voices in
the 1st measure is made on the 2nd note, but influences the 1st note.

In the 2nd measure, the pattern without the tie, leads to combined
voices.
  
"
	}
\version "2.21.0"

vone =

%%%%%%%%%%%%%%  0   1   2   3  
\relative {
  \time 2/4
  a'8[ a]  a8[ a] |
  a8[ a]  a8[ a]
}

vtwo =
\relative {
  \time 2/4
  f'8[ f]~ 8[ f] |
  f8[ f]  f8[ f]
}


\partCombine \vone \vtwo

