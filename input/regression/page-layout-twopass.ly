\header
{
  texidoc = "Page breaking details can be stored for later reference." 
}

\version "2.10.0"

\paper  {
  #(define write-page-layout #t)
}
bla = \new Staff {
  c1 c1
  \break
  \grace { c16 } c1\break
  \repeat unfold 5 \relative { c1 c1 c1 }
}

\book {
  \score {
    \bla
    \layout {
      #(define tweak-key "blabla")
    }
  }
}

tweakFileName = #(format "~a-page-layout.ly" (ly:parser-output-name parser))

#(newline)

%% unfortunately: the following doesn't show up in lp-book output

%% if the following is there, the tweak file will be overwritten.

%{

#(ly:progress "Including file: '~a'" tweakFileName)

\markup {
  \column {
    \line { contents of \typewriter tweakFileName: }
    \smaller \verbatim-file #tweakFileName
  }
}

%}
