\version "2.12.0"

\header {
  lsrtags = "pitches, text"
  texidoc = "
Internally, @code{\\ottava} sets the properties @code{ottavation}
(for example, to @code{\"8va\"} or @code{\"8vb\"}) and
@code{middleCPosition}.  To override the text of the bracket, set
@code{ottavation} after invoking @code{\\ottava}.
"
  doctitle = "Ottava text"
}

{
  \ottava #1
  \set Staff.ottavation = #"8"
  c''1
  \ottava #0
  c'1
  \ottava #1
  \set Staff.ottavation = #"Text"
  c''1
}
