\version "2.27.1"

\header {
  texidoc = "A time signature change before a grace note in one staff is
synchronized with the same time change in a staff without grace notes."
}

#(ly:set-option 'warning-as-error #t)

\layout { ragged-right = ##t }

<<
  \new Staff { c'1 \time 3/4 \grace e'8 d'4 e' f' }
  \new Staff { c'1 \time 3/4 d'4 e' f' }
>>
