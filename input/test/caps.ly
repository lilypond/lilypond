
\version "2.1.19"
\header {
texidoc =  "@cindex Capital Letters
You can set the font to use small caps.
" }

shapeSC   = \property LyricsVoice.LyricText \override #'font-shape = #'caps
shapeNorm = \property LyricsVoice.LyricText \revert   #'font-shape

\score { <<
  \notes \relative c'' { c4 c4 c8 c8 c8 }
  \lyrics \context LyricsVoice { 
    what4 is4 \shapeSC The8  Ma -- trix? }
  >>
  \paper { raggedright = ##t}
}

