
\version "2.19.21"

\header {

texidoc="Bar numbers may be set and their padding adjusted individually.  
The counting of bar numbers is started after the anacrusis.

To prevent clashes at the beginning of a line, the padding may have to 
be increased.
"

}

\layout {
  ragged-right = ##t
}

\relative {
  \override Score.BarNumber.break-visibility = #all-visible
  \partial 4 c''4 
  c1 c c
  \set Score.currentBarNumber = #99999
  \override Score.BarNumber.padding = #3
  c1 c
}
