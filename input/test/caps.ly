
\version "2.2.0"
\header {
texidoc = "@cindex Capital Letters
The font can be changed to small caps.
" }

shapeSC = \override LyricText  #'font-shape = #'caps
shapeNorm = \revert LyricText #'font-shape

\score { <<
  \notes \relative c'' { c4 c4 c8 c8 c8 }
  \context Lyrics \lyrics { 
    what4 is4 \shapeSC The8  Ma -- trix? }
  >>
  \paper { raggedright = ##t}
}

