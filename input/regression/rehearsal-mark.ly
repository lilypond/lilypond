

\header { texidoc= " Rehearsal marks are printed over barlines. They
can be incremented automatically or manually.  "}


\version "1.9.4"


global =  \notes {
  s1 | \mark "A"
  s1 | \mark \default 
  s1 | \mark \default 
  s1 | \mark "12"
  s1 | \mark \default 
  s1 | \mark "A2"
  s1 | \mark \markup { mark \column < up \bold down > }
  s1
}

one =  \notes \relative c {
  c''1 c c c c c c 
}


\score{
\context Staff	<< \global \one >>
}

