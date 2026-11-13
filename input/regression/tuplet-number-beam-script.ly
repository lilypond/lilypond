\version "2.27.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "When a beam makes the tuplet bracket redundant, scripts that
the bracket no longer needs to clear must not raise the tuplet number.
Only a script directly below the number moves it.  In the first three
triplets the accent is on the first, second and third note respectively:
only the second triplet's number is raised.  The last triplet has a visible
bracket (no beam) and keeps clearing the accent."
}

\layout { ragged-right = ##t }

{
  \time 5/4
  \tuplet 3/2 { a'8^> a' a' }
  \tuplet 3/2 { a'8 a'^> a' }
  \tuplet 3/2 { a'8 a' a'^> }
  \tuplet 3/2 { a'4^> a' a' }
}
