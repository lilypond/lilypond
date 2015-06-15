\version "2.19.22"


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
<c e gis>1
<c es ges>1
<c es ges bes>1
<c es ges beses>1
<c e g bes c'  d' e'>1
<c e g a bes>1
<c e g a d'>1
<c e g b fis'>1
<c e g bes des' ees' fis' aes'>1
}


<<
  \new ChordNames {
    %%	#(set-chord-name-style 'ignatzek)
    \chs
  }
  \new Staff  \transpose c c' { \chs }
>>
