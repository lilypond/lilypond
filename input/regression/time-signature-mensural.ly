\version "2.19.21"

\header {
  texidoc = "This test covers the mensural and neomensural time signature styles."
}

% N.B. It's strange that these warnings are doubled.
#(ly:expect-warning "Cannot find glyph timesig.mensural11")
#(ly:expect-warning "Cannot find glyph timesig.mensural11")
#(ly:expect-warning "Cannot find glyph timesig.neomensural11")
#(ly:expect-warning "Cannot find glyph timesig.neomensural11")

\layout { indent = 0 }

testMusic = \relative {
    \time 4/4 \partial 4 f'4
    \time 2/2 \partial 4 f4
    \time 6/4 \partial 4 f4
    \time 6/8 \partial 4 f4
    \time 3/2 \partial 4 f4
    \time 3/4 \partial 4 f4
    \time 9/4 \partial 4 f4
    \time 9/8 \partial 4 f4
    \time 4/8 \partial 4 f4
    \time 2/4 \partial 4 f4
    \time 1/1 \partial 4 f4
}

\new Staff {
    \override Staff.TimeSignature.break-visibility = ##(#f #t #t)
    \override Staff.TimeSignature.style = #'mensural
    \testMusic \break
    \override Staff.TimeSignature.style = #'neomensural
    \testMusic \break
}
