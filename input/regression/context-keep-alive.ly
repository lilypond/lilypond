\version "2.27.6"

\header {
  texidoc = "The second @code{Voice} keeps its style when it is reentered
because @code{keepAlive} is set; without it the context would be recreated
with default settings."
}

\layout { ragged-right = ##t }
#(ly:set-option 'warning-as-error #t)

music = \fixed c' {
  \context Voice = AAA {
    \voiceOneStyle
    c4 d e f
  }
  \context Voice = BBB {
    \voiceTwoStyle
    \set Voice.keepAlive = ##t
    g a b c
  }
  \context Voice = AAA { c1 }
  \context Voice = BBB { c1 }
  \context Voice = AAA { c1 }
}

\score {
  \new Staff \music
}
