\version "2.19.21"
\header{
  texidoc="
In addition to normal collision rules, there is support for polyphony, 
where the collisions are avoided by shifting middle voices horizontally.
"
}


twovoice =
\relative \context Staff  << 
  {  g4 f f e e d d c } 
  \\ {  c4 c  d d e e f f }
>>

twovoicechords = \context Staff  <<
  \relative {
    e'4 d c b a g f
  }\\
  \relative {
    <a c>4 <a c>4 <a c>4 <a c>4 <a c>
    <a c> <a c>  
  }
>>


threevoice = \context Staff  <<
  { g4 f e f g a g2 } \\
  {  c4 d e d c d es } \\
  { e4 e e e e e e e  }
>>

hairyChord = \context Staff  \relative
<<
  e'' \\
  fis, \\
  cis' \\
  \\
  ais
>>

minims = <<
  { e''2 e'' e' d' } \\
  c'' \\
  { c' c' c' c' } \\
  g' >>

semibreves = <<
  { e''1 e'' e' d' } \\
  c'' \\
  { c' c' c' c' } \\
  g' >>

sequence = <<
  \new Voice { \voiceOne g''1 e''2 d'' e'' d'' }
  \new Voice { \voiceThree c''2 b' a'1 b' }
  \new Voice { \voiceFour g'1 e'2 f'2 a'1 }
  \new Voice { \voiceTwo c'1 c'2 d' e'2 d' }
>>

  {
    \transpose c c' {
      \twovoice
      \twovoicechords
      \threevoice
    }
    \hairyChord
    \break
    \minims \bar "||"
    \semibreves \bar "||"
    \sequence
  }

