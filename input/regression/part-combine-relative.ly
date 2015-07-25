\version "2.19.23"

\header {
  texidoc = "@code{\\partcombine} needs to be given pitches in their
final octaves, so if @code{\\relative} is used it must be applied
inside @code{\\partcombine}.  The pitches in @code{\\partcombine} are
unaffected by an outer @code{\\relative}, so that the printed output
shows the pitches that @code{\\partcombine} used.

The expected output of this test is three identical measures."
}

\new Staff {
  \partcombine \absolute { e'2 f' } { c'2 d' }
  \relative \partcombine { e'2 f' } { c'2 d' } % relative is ignored
  \partcombine \relative { e'2 f } \relative { c'2 d }
}
