\version "2.23.4"

\header {
  texidoc = "Console output should indicate that translators created with
@code{make-translator} are available in @samp{\\layout} and @samp{\\midi},
engravers created with @code{make-engraver} just in @samp{\\layout},
and performers created with @code{make-performer} just in @samp{\\midi}."
}

translator =
#(make-translator
  ((finalize self)
   (ly:message "\nFinalizing translator.\n")))

engraver =
#(make-engraver
  ((finalize self)
   (ly:message "\nFinalizing engraver.\n")))

performer =
#(make-performer
  ((finalize self)
   (ly:message "\nFinalizing performer.\n")))


\score {
  \new Staff { \applyContext #(lambda _ (ly:message "\nIn \\layout:\n")) c'1 }
  \layout {
    \context { \Staff
	       \consists #translator
	       \consists #engraver
	       \consists #performer
	     }
  }
}

\score {
  \new Staff { \applyContext #(lambda _ (ly:message "\nIn \\midi:\n")) c'1 }
  \midi {
    \context { \Staff
	       \consists #translator
	       \consists #engraver
	       \consists #performer
	     }
  }
}
