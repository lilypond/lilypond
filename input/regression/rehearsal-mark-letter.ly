

\header { texidoc= "Rehearsal marks in letter style: the I is skipped,
and after Z, we continue with double letters.  The mark may be set
with @code{\mark NUMBER}, or with @code{Score.rehearsalMark}."
      }

\version "2.1.7"
\score {  \notes \relative c'' {
  c1  \mark \default
  c1  \mark #7
  c1  \mark \default
  c1  \mark \default
  c1  \mark \default
  \property Score.rehearsalMark = #24
  c1  \mark \default
  c1  \mark \default
  c1  \mark \default
  c1  \mark \default
  }
}
