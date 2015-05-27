\version "2.19.22"

\header {
  texidoc = "Music events can be extracted from a score with event
listeners."
  title = "Black-box Testing"
  composer = "Graham Percival"
}

#(define EVENT_LISTENER_CONSOLE_OUTPUT 1)

\include "event-listener.ly"



st =
#(define-music-function
  (text)
  (string?)
#{
  \override TextSpanner.bound-details.left.text = $text
#})


vlnone = \new Staff {
  \set Staff.instrumentName = "violin-1"
  \set Staff.midiInstrument = "violin"
  \override TextSpanner.style = #'line
  \override TextSpanner.bound-details.right.padding = #-1
  \override TextSpanner.bound-details.left.stencil-align-dir-y =
    #CENTER
  \override TextSpanner.bound-details.right.text =
    \markup { \draw-line #'(0 . -1) }
\relative {
  \key d \major
  \tempo 4 = 96

  a4\f d fis8-. a-. r4
  d16(\downbow cis b a) g4 \breathe e8\p( g) fis4 |

  e4\< \glissando g8 fis g4-_\mp\>
    \st "III"
    b8-_\startTextSpan a-_\stopTextSpan
    b4\p\<( d8 cis) d4(-. fis8-.^"II" e-.^"II")
  fis16(\mf\downbow g a b c\> b a g) fis(\upbow e d c) b(\downbow a g fis)
  e16\mp\upbow ~ e-.\mp r8 e'-.\upbow^"tip" r e,4->^"mb" r4 |

  \key d \minor
  \time 3/4
  \tempo 4 = 120
  d4.\mf^"pizz." e8 f4
  f'4. e8 d4
  d,4.\mp c8 bes4 |
  \tempo 4 = 88
  a16\p e' a e' a,,32\f e' a e' r8 r4
  d4^"arco"^"lh"\> \acciaccatura { c8 } bes4 \acciaccatura { a8 } g4
  \st "III"
  fis16\p\startTextSpan a_"II" g a_"II" a a_"II" bes a_"II"
    c a_"II" bes a_"II"\stopTextSpan
  a4\breathe a,\breathe r4 |
}


  \bar "|."
}

\score {
  << \vlnone >>
  \layout{}
  \midi{}
}
