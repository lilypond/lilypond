

\header { texidoc= "Rehearsal marks in letter style: the I is skipped,
and after Z, we continue with double letters.  The mark may be set
with @code{\mark NUMBER}, or with @code{Score.rehearsalMark}."
      }

\version "2.1.7"


global =  \notes {
  s1 | \mark #6
  s1 | \mark \default
  s1 | \mark \default
  s1 | \mark \default
  \property Score.rehearsalMark = #24
  s1 | \mark \default
  s1 | \mark \default
  }


one =  \notes \relative c {
  c''1 c c c c c c 
}


\score{
\context Staff	<< \global \one >>
}

