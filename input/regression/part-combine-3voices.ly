\version "2.21.0"

\header {
  texidoc ="It is possible to use the part combiner for three
	voices with \\partCombineUp and \\partCombineDown."
}


soprano = { d''2 f'' g'' }
alto = { a' c''4 d'' e''2 }
tenor = { f'2 a'4 b' c''2 }
basso = { d'4 e' f' g' g'2 }

\new Staff << \partCombineUp \soprano \alto \\ \basso >>

\new Staff << \soprano \\ \partCombineDown \tenor \basso >>


