\version "2.21.0"

\header {
  texidoc = "User code is not allowed to create a Global context.
  The visual output of this test is not important."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "cannot create context: ~a") 'Global)

\new Global s1
