#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
texidoc =  "@cindex Capital Letters
You can set the font to use small caps.
" }

shapeSC   = \property Lyrics.LyricText \override #'font-shape = #'caps
shapeNorm = \property Lyrics.LyricText \revert   #'font-shape

\score { <
  \notes \relative c'' { c4 c4 c8 c8 c8 }
  \lyrics \context Lyrics { 
    what4 is4 \shapeSC The8  Ma -- trix? }
  >
  \paper { raggedright = ##t}
}

