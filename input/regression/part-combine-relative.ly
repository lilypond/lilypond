\version "2.21.0"

\header {
  texidoc = "@code{\\partCombine} needs to be given pitches in their
final octaves, so if @code{\\relative} is used it must be applied
inside @code{\\partCombine}.  The pitches in @code{\\partCombine} are
unaffected by an outer @code{\\relative}, so that the printed output
shows the pitches that @code{\\partCombine} used.

The expected output of this test is three identical measures."
}

\new Staff {
  \partCombine \absolute { e'2 f' } { c'2 d' }
  \relative \partCombine { e'2 f' } { c'2 d' } % relative is ignored
  \partCombine \relative { e'2 f } \relative { c'2 d }
}
