
\version "2.1.22"
\header {
texidoc = "@cindex Capital Letters
You can set the font to use small caps.
" }

shapeSC = \override LyricText  #'font-shape = #'caps
shapeNorm = \revert LyricText #'font-shape

\score { <<
  \notes \relative c'' { c4 c4 c8 c8 c8 }
  \lyrics \context Lyrics { 
    what4 is4 \shapeSC The8  Ma -- trix? }
  >>
  \paper { raggedright = ##t}
}

