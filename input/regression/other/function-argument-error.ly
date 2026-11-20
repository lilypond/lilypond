\version "2.27.6"

\header {
  texidoc = "Argument errors for LilyPond's function types are
displayed with human-readable type and value in LilyPond syntax."
}

expect-error = ##t
#(ly:expect-warning (G_ "wrong type for argument ~a.  Expecting ~a, found ~a")
		   1 "pitch" "#4")
\fixed 4 { % <- missing pitch
  c'4.-> d'8 f'4 g'4
}
