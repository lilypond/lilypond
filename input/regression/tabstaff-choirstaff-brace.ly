\version "2.23.3"

\header {
  texidoc = "A @code{TabStaff} placed inside a @code{ChoirStaff} does not
have an extraneous bracket.  In this test, the two snippets should look
the same."
}

music = { c''1 }

\new StaffGroup <<
  \new TabStaff \music
  \new Staff \music
>>

\new ChoirStaff <<
  \new TabStaff \music
  \new Staff \music
>>
