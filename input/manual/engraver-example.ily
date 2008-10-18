%% texidoc = "Include file for engraver example."
\version "2.11.61"
topVoice =  \relative c' {
  \key d\major
  es8([ g] a[ fis])
  b4
  b16[-. b-. b-. cis-.]
  d4->
}

botVoice =  \relative c' {
  \key d\major
  c8[( f] b[ a)]
  es4
  es16[-. es-. es-. fis-.]
  b4->
}

hoom =  \relative c {
  \key d \major
  \clef bass
  g8-. r
  r4 
  fis8-.
  r8
  r4
  b'4->
}

pah =  \relative c' {
  r8 b-.
  r4
  r8 g8-.
  r16 g-. r8
  \clef treble
  fis'4->
}
