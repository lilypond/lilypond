\version "1.5.68"


global =  \notes {
  s1 | \mark "A"
  s1 | \mark \default 
  s1 | \mark \default 
  s1 | \mark "12"
  s1 | \mark \default 
  s1 | \mark "A2"
%% FIXME  s1 | \mark #'(music "scripts-segno")  
  s1
}

one =  \notes \relative c {
  c''1 c c c c c c 
}


\score{
\context Staff	< \global \one >
}
