\version "2.21.0"

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning "Could not find ottavation markup for ~a octaves up." 3)
#(ly:expect-warning "Could not find ottavation markup for ~a octaves up." -3)

\header {
  texidoc = "Ottavation markups can be changed by the user.
LilyPond warns about missing markups (in this example for
+3 and -3 octaves)."
}

{
  \set Staff.ottavationMarkups =
  #`(( 2 . "16")
     ( 1 . ,#{ \markup \concat \general-align #Y #UP { "8" \tiny "va" } #})
     (-1 . "8")
     (-2 . ,#{ \markup \concat { "15" \tiny "ma bassa" } #}))
  c''4 4 4 4
  \ottava 1
  c'''4 4 4 4
  \ottava 2
  c''''4 4 4 4
  \ottava 3
  c'''''4 4 4 4
  \ottava -1
  c'4 4 4 4
  \ottava -2
  c4 4 4 4
  \ottava -3
  c,4 4 4 4
  \ottava 0
  c''4 4 4 4
}
