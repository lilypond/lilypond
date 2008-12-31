\version "2.12.0"


\header {
texidoc = "Jazz chords may have unusual combinations."
}

chs =  
{
<c d  f g>1
<c d  e f g>1
<c d  e  g>1
<c d es  g as>1
<c d e f g bes d' f'>1
<c d e f g bes c'  d' e'>1
<c e g bes c'  d' e'>1
<c e g a bes>1
<c e g a d'>1
}


<<
  \context ChordNames {
    %%	#(set-chord-name-style 'ignatzek)
    \chs
  }
  \context Staff  \transpose c c' { \chs }
>>
