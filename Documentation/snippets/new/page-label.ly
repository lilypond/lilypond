\version "2.25.35"

\header {
  categories = "Spacing"

  texidoc = "
Page labels may be placed inside music or at top-level, and referred to
in markups.
"

  doctitle = "Page label"
} % begin verbatim


#(set-default-paper-size "a7" 'landscape)
#(set-global-staff-size 11)

\label license
\markup \fill-line {
  \center-column {
    "This snippet is available"
    "under the Creative Commons"
    "Public Domain Dedication license." } }

{
  \repeat volta 2 {
    \label startRepeat
    \*22 { c'2 2 }
    \pageBreak
    \*16 { c'2 2 }
  }
  \textEndMark \markup {
    \with-link #'startRepeat \line {
      To page \page-ref #'startRepeat "0" "?"
    }
  }
}

\markup \fill-line {
  \line {
    See page \page-ref #'license "0" "?" for
    licensing information. } }
