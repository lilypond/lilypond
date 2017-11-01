\header {

    texidoc = "Dots of rests should follow the rest positions. "

}

\version "2.18.0"

\layout { indent = 0 }
\paper { ragged-right = ##t }

mus = {
  r\longa. r\breve.
  r1. r2. r4.
  \once \override Rest.style = #'classical r4.
  \once \override Rest.style = #'z r4.
  r8. r16. r32. r64. r64.
}

vI = \new Voice { \voiceOne   \mus }
vII = \new Voice { \voiceTwo   \mus }
vIII = \new Voice { \voiceThree \mus }
vIV = \new Voice { \voiceFour  \mus }

{
  \set Score.timing = ##f
  \set Score.initialTimeSignatureVisibility = ##(#f #f #f)
  \mus
  \bar "" \break
  << \vI \vII >>
  \bar "" \break
  << \vI \vII \vIII >>
  \bar "" \break
  << \vI \vII \vIII \vIV >>
}
