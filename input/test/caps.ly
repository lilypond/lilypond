
\version "2.7.39"
\header {
texidoc = "@cindex Capital Letters
The font can be changed to small caps.
" }

shapeSC = \override LyricText  #'font-shape = #'caps
shapeNorm = \revert LyricText #'font-shape

\score { <<
   \relative c'' { c4 c4 c8 c8 c8 }
  \context Lyrics \lyricmode { 
    what4 is4 \shapeSC The8  Ma -- trix? }
  >>
  \layout { ragged-right = ##t}
}

