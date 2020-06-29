\version "2.21.3"

\header {
  texidoc = "When an unpitched duration is parsed as a rhythmic event,
it sets the default duration of the following note events.  This happens
even when it is the argument of a music function.  In these examples, notes
with an explicit duration are indicated with an accent and the following notes
have to have the same duration."
}

\layout {
  ragged-right = ##t
}


same =
#(define-music-function (x) (ly:music?)
  #{ #x -! #})


\fixed c' { 2-! c \same c4 c \same 2 c c \bar "|." | }
