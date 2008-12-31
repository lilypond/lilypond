\version "2.12.0"
\header{
  texidoc="
In addition to normal collision rules, there is support for polyphony, 
where the collisions are avoided by shifting middle voices horizontally.
"
}


twovoice =
\relative c' \context Staff  << 
  {  g4 f f e e d d c } 
  \\ {  c4 c  d d e e f f }
>>

twovoicechords = \context Staff  <<
  \relative c' {
    e4 d c b a g f
  }\\
  \relative c' {
    <a c>4 <a c>4 <a c>4 <a c>4 <a c>
    <a c> <a c>  
  }
>>


threevoice = \context Staff  <<
  { g4 f e f g a g2 } \\
  {  c4 d e d c d es } \\
  { e4 e e e e e e e  }
>>

hairyChord = \context Staff  \relative c''
<<
  e \\
  fis, \\
  cis' \\
  \\
  ais
>>


  {
    \transpose c c' {
      \twovoice
      \twovoicechords
      \threevoice
    }
    \hairyChord
  }
  

