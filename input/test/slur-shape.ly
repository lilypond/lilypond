#(ly:set-option 'old-relative)
\version "1.9.2"
\header {
    texidoc = "Slurs become flatter as they grow longer. "

}

x = {
  \outputproperty #(make-type-checker 'note-head-interface) 
    #'extra-offset = #'(-1 . 0)
}

\score {
    \context Staff \notes\relative c <
    \new Voice { \x f(f) }
    \new Voice { \x g(s4g) }
    \new Voice { \x a(s4*2a) }
    \new Voice { \x b(s4*3b) }
    \new Voice { \x c(s4*4c) }
    \new Voice { \x d(s4*5d) }
    \new Voice { \x e(s4*6e) }
    \new Voice { \x f(s4*7f) }
    \new Voice { \x g(s4*8g) }
    \new Voice { \x a(s4*9a) }
    \new Voice { \x b(s4*10b) }
    \new Voice { \x c(s4*11c) }
    \new Voice { \x d(s4*12d) }
    \new Voice { \x e(s4*13e) }
    \new Voice { \x f(s4*14f) }
    \new Voice { \x g(s4*15g) }
    \new Voice { \x a(s4*16a) }
    \new Voice { \x b(s4*17b) }
    \new Voice { \x c(s4*18c) }
    \new Voice { \x d(s4*19d) }
    \new Voice { \x e(s4*20e) }
    \new Voice { \x f(s4*21f) }
    \new Voice { \x g(s4*22g) }
    \new Voice { \x a(s4*23a) }
    \new Voice { \x b(s4*24b) }
    \new Voice { \x c(s4*25c) }
    \new Voice { \x d(s4*26d) }
  >
  \paper {
    raggedright = ##t
    \translator{
      \VoiceContext
      Slur \override #'direction = #1
      Stem \override #'direction = #-1
    }
  }
}

