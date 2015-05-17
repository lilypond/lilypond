%% texidoc = "Include file for engraver example."
\version "2.19.21"
topVoice =  \relative {
  \key d \major
  es'8([ g] a[ fis])
  b4
  b16[-. b-. b-. cis-.]
  d4->
}

botVoice =  \relative {
  \key d \major
  c'8[( f] b[ a)]
  es4
  es16[-. es-. es-. fis-.]
  b4->
}

hoom =  \relative {
  \key d \major
  \clef bass
  g,8-. r
  r4 
  fis8-.
  r8
  r4
  b'4->
}

pah =  \relative {
  r8 b-.
  r4
  r8 g8-.
  r16 g-. r8
  \clef treble
  fis'4->
}
